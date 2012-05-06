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
import net.vrijheid.clouddrive.config._
import net.vrijheid.clouddrive.exceptional._
import net.vrijheid.clouddrive.control._
import voldemort.versioning._
import java.io.{Serializable}

package net.vrijheid.clouddrive.pipes {
	
	//
	class Quotas[T](implicit val ctx: RootContext[T]) extends PipeItem with Serializable  {
		
		//TBD: This should be userbased maybe (how we measure quotas), eventually. If we merge on the config level, we stand a good chance.
		private val quota_meterer = VMTalk getQuotaMeterer;
		private implicit val gbmonth_meterer = VMTalk getGBMonthMeterer	
			
		private def initGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {
			
			debug("In initGBMonth")
			//The idea is that we get the previous month, read it and set the current months totals that way
			//if there is no previous month, then this month is the first, and we initialize it with zeros
			val current_month = userMonthYear(user)
			val previous_month = previousUserMonthYear(user)
			val previous_gb_month = gbm get_? previous_month match {
				case Some(v) => v
				case None => {
					//First month
					debug("new month, first time")
					gbm init(current_month,new GBMonth(0,0))					
				}
			}
			val gbmonth = previous_gb_month getValue;
			debug("updating usage for new month")
			gbmonth.usage = (gbmonth.total_size * hoursOfThisMonth)
			debug("... and init on the new month with the updated values")
			gbm init(current_month,gbmonth)

		}
		
		private def getGBMonth(user: String)(implicit gbm: VMClient[String,GBMonth]): Versioned[GBMonth] = {
			
			debug("In getGBMonth")
			//We get the current month, if there is none there, we create one based on the previous month (initGBMonth)
			val current_month = userMonthYear(user)
			gbm get_? current_month match {
				case Some(v) => v
				case None => {
					debug("no current_gb_month yet, call initGBMonth")
					initGBMonth(user)(gbm)					
				}
			}

		}	
		
		//Initialization, if needed
		override def ||()  {
			
			val quota_type = ctx.userConfig("quota_type","storage")
			val who = userYear(ctx.user)
			debug ("Getting quota for " + who)
			//Get how much we have used as GBMonth
			Thread.sleep(0)
			val used = getGBMonth(ctx.user)(gbmonth_meterer)
			debug("quota_meterer is null?" + (quota_meterer == null))

			//Get the Quotum and cast it
			val quotum = quota_type match {
				
				case "storage" => {
					debug("In THREAD: quota_type is storage, returning value")
					val q = quota_meterer.getValue(who)
					q.asInstanceOf[StorageQuotum]
				}
				
				case _ => {
					debug("quota_type undetermined, returning value")
					quota_meterer.getValue(who).asInstanceOf[StorageQuotum]
				}
			}
			//Throw an exception, right away. Now we can rite a 404 at the place where we handle this.
			if (quotum quotumReached used.getValue) {
				throw new QuotaException
			}
			
		}
		
		//"commit" outgoing stream (finalize action)
		override def |<|(data: Array[Byte]): Array[Byte] = {

			val quota_type = ctx.userConfig("quota_type","storage")
			val who = userYear(ctx.user)
			debug ("Getting quota for " + who)
			//Get how much we have used as GBMonth
			val used = getGBMonth(ctx.user)(gbmonth_meterer)
			//Close VMCLients
			//gbmonth_meterer.factory.close;
			//quota_meterer.factory.close;						
			data
		}		
	}
	
}