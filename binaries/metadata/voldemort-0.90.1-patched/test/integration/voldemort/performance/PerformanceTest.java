/*
 * Copyright 2008-2009 LinkedIn, Inc
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package voldemort.performance;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import voldemort.TestUtils;
import voldemort.utils.Time;

public abstract class PerformanceTest {

    private final AtomicInteger numberOfFailures = new AtomicInteger(0);
    private long elapsedTimeNs;
    private long[] operationTimes;
    private volatile boolean hasCompleted;
    private int numberOfThreads;

    public abstract void doOperation(int index) throws Exception;

    public void setUp() {
    // override me to do stuff
    }

    public void tearDown() {
    // override me to do stuff
    }

    public void run(int numRequests, int numThreads) {
        setUp();
        try {
            this.numberOfThreads = numThreads;
            this.hasCompleted = false;
            this.numberOfFailures.set(0);
            this.operationTimes = new long[numRequests];
            ExecutorService executor = Executors.newFixedThreadPool(numThreads);
            final CountDownLatch latch = new CountDownLatch(numRequests);
            final AtomicInteger index = new AtomicInteger(0);

            long start = System.nanoTime();
            for(int i = 0; i < numRequests; i++) {
                executor.execute(new Runnable() {

                    public void run() {
                        int current = index.getAndIncrement();
                        long begin = System.nanoTime();
                        try {
                            doOperation(current);
                        } catch(Exception e) {
                            numberOfFailures.getAndIncrement();
                            e.printStackTrace();
                        } finally {
                            operationTimes[current] = System.nanoTime() - begin;
                            latch.countDown();
                        }
                    }
                });
            }

            try {
                latch.await();
            } catch(InterruptedException e) {
                e.printStackTrace();
            }

            this.hasCompleted = true;
            this.elapsedTimeNs = System.nanoTime() - start;
            executor.shutdownNow();
            try {
                executor.awaitTermination(3, TimeUnit.SECONDS);
            } catch(InterruptedException e) {}
        } finally {
            tearDown();
        }
    }

    public void printStats() {
        checkComplete();
        System.out.println("Total number of operations: " + this.operationTimes.length);
        System.out.println("Total elapsed seconds: " + this.elapsedTimeNs
                           / (double) Time.NS_PER_SECOND);
        System.out.println("Number of failures: " + this.numberOfFailures.get());
        System.out.println("Number of threads: " + this.numberOfThreads);
        System.out.println("Avg. operations/second: " + getOperationsPerSecond());
        System.out.println("Average time: " + getAverageOperationTimeMs() + " ms");
        System.out.println("Std dev.: " + getStandardDeviationMs() + " ms");
        System.out.println("Median time: " + getOperationTimeMsQuantile(0.5d) + " ms");
        System.out.println("1st percentile: " + getOperationTimeMsQuantile(0.01d) + " ms");
        System.out.println("99th percentile: " + getOperationTimeMsQuantile(0.99d) + " ms");
    }

    public double getOperationsPerSecond() {
        checkComplete();
        double elapsedSeconds = this.elapsedTimeNs / (double) Time.NS_PER_SECOND;
        return this.operationTimes.length / elapsedSeconds;
    }

    public double getOperationTimeMsQuantile(double quantile) {
        checkComplete();
        return TestUtils.quantile(this.operationTimes, quantile) / (double) Time.NS_PER_MS;
    }

    public double getAverageOperationTimeMs() {
        checkComplete();
        double mean = TestUtils.mean(this.operationTimes);
        return mean / Time.NS_PER_MS;
    }

    public double getStandardDeviationMs() {
        checkComplete();
        double mean = TestUtils.mean(this.operationTimes);
        double sum = 0.0;
        for(int i = 0; i < this.operationTimes.length; i++)
            sum += (this.operationTimes[i] - mean) * (this.operationTimes[i] - mean);
        return Math.sqrt(sum / this.operationTimes.length) / Time.NS_PER_MS;
    }

    private void checkComplete() {
        if(!hasCompleted)
            throw new RuntimeException("Hasn't finished running yet!");
    }

}
