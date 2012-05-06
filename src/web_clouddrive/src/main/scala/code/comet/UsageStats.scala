/*
 * Cloud drive website
 * 
 * Copyright (c) 2010-2012, vrijheid.net

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import net.liftweb._
import common._
import http._
import util._
import Helpers._
import java.util.{Date}
import net.liftweb.http.js.JsCmds._
import scala.xml._
import net.vrijheid.clouddrive.utils._
import net.vrijheid.clouddrive.website.code.lib.login.{Login}
import net.vrijheid.clouddrive.website.code.lib.files.{LiftMetering}

package net.vrijheid.clouddrive.website.code.comet {
	
	class Usage extends CometActor with LiftMetering {
		
		override def defaultPrefix = Full("db")
	
		val h = new Handy()
		val user = Login.getUserEmail
	
		val kb = 1024L
		val mb = kb * kb
		val gb = kb * mb
		val month_days = h daysOfThisMonth;

		//Init the GB Month meterer and 
		implicit var gbmonth_meterer = VMTalk getGBMonthMeterer
		val quota_meterer = VMTalk getQuotaMeterer
		val quotum = (quota_meterer.getValue_?(userYear(user.openOr("")))) match {
			case Some(q) => q.asInstanceOf[StorageQuotum]
			case _ => new StorageQuotum
		}
		//quota_meterer.factory.close();
		
		override def lifespan = Full(5 seconds)
		
		def main_dashboard = (<span id="main-usage" class="usage-style">{S ? "Loading Dashboard..."}</span>)
		
		def render = bind("main" -> main_dashboard)

		def getUsageData = {
			
			//Note, you could add other data, such as requests, traffic etc, here as well. If you like....
			
			//TBD: use Failure here
			val now = h.userMonthYear(user.openOr(""))
			//Make sure the GBMonth meterer is up to date
			ensureMetering(user.openOr(""))
			val current_usage = gbmonth_meterer getValue(now)
			val total_size = current_usage.total_size;
			val total_size_mb: String = "%.3f".format(total_size.toDouble / mb)
			val percentage: String = quotum.quotumPercentage(current_usage).toString
			//val total_size_gb: String = "%.9f".format(total_size.toDouble / gb)
			
			<div> 
			<br/><br/>
			{S ? "Using "} {total_size_mb} MB {S ? "of"} {quotum.storage_quotum / gb} GB ({percentage} %).
			</div>
		}
		
		ActorPing.schedule(this, Tick, 5000L)
		
		// schedule a ping every N seconds so we redraw
		override def lowPriority : PartialFunction[Any, Unit] = { 
			case Tick => {
				try {
					partialUpdate(SetHtml("main-usage",getUsageData)) 
				} catch { 
				
					case e: Exception => {
						//gbmonth_meterer.factory.close;
						gbmonth_meterer = VMTalk getGBMonthMeterer;
					}
				
				}
				// schedule an update in 10 seconds 
				ActorPing.schedule(this, Tick, 2000L)
			}
		}
		
		override def localShutdown() {
			//gbmonth_meterer.factory.close;
		}
	} 
	case object Tick
}