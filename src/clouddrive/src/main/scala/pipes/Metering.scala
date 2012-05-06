/*
 * Cloud backed storage
 * 
 * Copyright (c) 2010-2012, vrijheid.net
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * *    Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 * *    Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 * *    Neither the name of vrijheid.net nor the
 *      names of its contributors may be used to endorse or promote products
 *      derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.httpsupport._
import net.vrijheid.clouddrive.control._
import scala.runtime._
import voldemort.versioning._
import net.vrijheid.clouddrive.providers._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes {
	
	//
	class Metering[T](implicit ctx: RootContext[T]) extends MetaData[T] with PipeItem  with Serializable{
		
		private var incoming_data: Long = 0
		private var outgoing_data: Long = 0
		private var request_counter: Long = 0
		private var storage_delta: Long = 0
		private val zero = 0.asInstanceOf[Long];
		private val one = 1.asInstanceOf[Long]
		private var storage_length: Long = 0;
		
		//This is he function to correct when having stale data: simply read again and add orur data
		val add_stale_update = {(current: Long, delta: Long) => {current + delta}}
		//This is a first class function to change the GBMonth couters
		
		val add_gbmonth_update = {
			(current: GBMonth,delta: (Long,Long)) => {
				debug("in add_gbmonth_update delta function")
				//delta is a tuple of (size,usage)
				current.total_size += delta._1;
				current.usage += delta._2;
				debug("new value is: " + current.toString)
				current
			}
		}
		
		private def sizeOfPath(path: String): Long = {
			//val path = "/" + ctx.user + ctx.verb.header("resource")
			//We get the tree, map each path to its length, and sum using foldLeft
			((treeAsList(path)).map { x => getPutLength(x)}).foldLeft(zero){ (acc: Long,x: Long) => {acc + x}}
		}
		
		private def totalSize(): Long = {
			sizeOfPath(stripTrailingSlash("/" + ctx.user))
		}
		
		private def initGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {
			
			debug("In initGBMonth")
			//The idea is that we get the previous month, read it and set the current months totals that way
			//if there is no previous month, then this month is the first, and we initialize it with zeros
			val current_month = userMonthYear(user)
			val previous_month = previousUserMonthYear(user)
			val previous_gb_month = (gbm get_? previous_month) match {
				case Some(v) => v.getValue
				case None => {
					debug("new month, first time")
					(gbm init(current_month,new GBMonth(0,0))).getValue
				}
			}

			val gbmonth = previous_gb_month
			debug("updating usage for new month")
			gbmonth.usage = (gbmonth.total_size * hoursOfThisMonth)
			debug("... and init on the new month with the updated values")
			gbm init(current_month,gbmonth)
		}
		
		private def initNextGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {
			
			debug("In initNextGBMonth")
			//The idea is that we get the previous month, read it and set the current months totals that way
			//if there is no previous month, then this month is the first, and we initialize it with zeros
			val current_month = userMonthYear(user)
			val next_month = nextUserMonthYear(user)
			val next_gb_month = (gbm get_? current_month) match {
				case Some(v) => v.getValue
				case None => {
					debug("new month without predecessor, initialzing")
					(gbm init(next_month,new GBMonth(0,0))).getValue	
				}
			}
			debug("current,next month keys = " + current_month + " , " + next_month)

			//Initialize with the values of last month
			debug("new month, with predecessor")
			val gbmonth = next_gb_month;
			gbmonth.usage = (gbmonth.total_size * hoursOfNextMonth)
			debug("gbmonth for next month will be: " + gbmonth.toString)
			gbm init(current_month,gbmonth)

		}
		
		private def getGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {
			
			debug("In getGBMonth")
			//We get the current month, if there is none there, we create one based on the previous month (initGBMonth)
			val current_month = userMonthYear(user)
			(gbm get_? current_month) match {
				case Some(v) => v
				case None => {
					debug("no current_gb_month yet, call initGBMonth")
					initGBMonth(user)					
				}
			}
		}
				
		//Initialization, if needed
		override def ||()  {
			incoming_data += (ctx.verb.header.getOrElse("header_length","0")).toLong
			val path = stripTrailingSlash("/" + ctx.user + ctx.verb.header("resource"))
			ctx.verb match {
				case x: DELETE => { storage_length = sizeOfPath(path)}
				case x: COPY => { storage_length = sizeOfPath(path)}
				case x: PUT => {
					//If this is a file overwrite compensate
					if(exists(path) && (! isCollection(path))) {
						storage_length -= getPutLength(path)
					}
				}
				case x: POST => {
					//If this is a file overwrite compensate
					if(exists(path) && (! isCollection(path))) {
						storage_length -= getPutLength(path)
					}
				}
				case _ => {}
			}
			
		}
		
		//Push data incoming
		override def >| (data : Array[Byte]) : Array[Byte] = {
			incoming_data += data.length;
			data
		}
		
		//"commit" incoming stream (finalize icnoming data metering)
		override def |>| (data : Array[Byte]): Array[Byte] = { 
			//First, the final addition
			incoming_data += data.length
			val moi_now = userMonthYear(ctx.user)
			val moi_year = userYear(ctx.user)
			//Get client for storing in voldemort
			val incoming_meterer = VMTalk getIncomingMeterer;
			//Initialize or current value
			incoming_meterer init(moi_now,zero)
			incoming_meterer init(moi_year,zero)
			//And add to the incoming meterer
			incoming_meterer applyDelta(moi_now,incoming_data,add_stale_update)
			incoming_meterer applyDelta(moi_year,incoming_data,add_stale_update)
			//CLose the incoming client
			//incoming_meterer.factory.close;
			data 
		}
		//Push outgoing data
		override def <| (data : Array[Byte]) : Array[Byte] = {
			debug("Metering, data length in <| = " + data.length)
			outgoing_data += data.length;
			data
		}
		
		
		//"commit" outgoing stream (finalize action)
		override def |<|(data: Array[Byte]): Array[Byte] = {
			//First, the final addition
			outgoing_data += data.length;
			//Now we are hoing to add to the outgoing bandwith
			val moi_now = userMonthYear(ctx.user)
			val moi_year = userYear(ctx.user)			
			//Get client for storing in voldemort
			val outgoing_meterer = VMTalk getOutgoingMeterer;
			//Initialize or current value
			outgoing_meterer init(moi_now,zero)
			outgoing_meterer init(moi_year,zero)
			//And add to the incoming meterer
			outgoing_meterer applyDelta(moi_now,outgoing_data,add_stale_update)
			outgoing_meterer applyDelta(moi_year,outgoing_data,add_stale_update)
			
			//Now, we're going to up the rquest counter
			val request_meterer = VMTalk getRequestMeterer;
			//Initialize or current value
			//val request_count : Int = 1 + (request_meterer getvalue moi)
			//And add to the incoming meterer
			request_meterer init(moi_now,zero)
			request_meterer init(moi_year,zero)
			request_meterer applyDelta(moi_now,one,add_stale_update)
			request_meterer applyDelta(moi_year,one,add_stale_update)
			
			
			//GBMonth storage metering
			//Distinguish based on verb, i.e. COPY, DELETE, PUT, POST, default cases...
			implicit val gbmonth_meterer = VMTalk getGBMonthMeterer;
			var hours = (hoursOfThisMonth - currentMonthHour)
			debug("hours left this month: " + hours.toString)
			var key = userMonthYear(ctx.user);
			debug("Key = " + key)
			//We only do this so that the actual key/value for this month is initialized
			val month: Versioned[GBMonth] = hours match {
				
				case 0  => {
					debug("No more hours left this month, get hours of next month")
					//We are at the end of the month, so or #hours is the hours of next month
					//Key needs to change, tpoo
					hours = hoursOfNextMonth
					debug("Updating key to next month")
					key = nextUserMonthYear(ctx.user);
					debug("...and initalizing next month")
					initNextGBMonth(ctx.user)
				}
				
				case _ => {
					debug("hours left, get the current GBMonth")
					getGBMonth(ctx.user)
				}
			}
			//Now adapt the GBMonth object differently per HTTP verb
			ctx.verb match {
				//In general: get the value from the versioned GBMonth
				//Then change the total_size and usage according to the verb i.e.
				//subtract for a delete, add fpr COPY, POST, PUT
				case x: DELETE => {
					val delta : (Long,Long) = (-1 * storage_length, -1 * (storage_length * hours))
					debug("DELETE, delta = " + delta)
					gbmonth_meterer applyDelta(key,delta,add_gbmonth_update)
				}
				case x: COPY => {
					val delta: (Long,Long) = (storage_length,(storage_length * hours))
					debug("COPY, delta = " + delta)
					gbmonth_meterer applyDelta(key,delta,add_gbmonth_update)
				}
				case x: PUT => {
					//This way, we correct for oerwrotes of data accidentally doubling usage
					//storage_length is negatve as per the || then
					val length = ctx.putContentLength + storage_length
					val delta: (Long,Long) = (length,(length * hours))
					debug("POST, delta = " + delta)
					gbmonth_meterer applyDelta(key,delta,add_gbmonth_update)
				}
				case x: POST => {
					//This way, we correct for oerwrotes of data accidentally doubling usage
					//storage_length is negatve as per the || then
					val length = ctx.putContentLength + storage_length
					val delta: (Long,Long) = (length,(length * hours))
					debug("POST, delta = " + delta)
					gbmonth_meterer applyDelta(key,delta,add_gbmonth_update)
				}
				//TODO: handle WEBSITE as per Lift
				case _ => {}
			}
					
			//Close all open VMClients
			//outgoing_meterer.factory.close;
			//request_meterer.factory.close;		
			//gbmonth_meterer.factory.close;	
			//And pass the data
			data
		}		
		
	}
	
}