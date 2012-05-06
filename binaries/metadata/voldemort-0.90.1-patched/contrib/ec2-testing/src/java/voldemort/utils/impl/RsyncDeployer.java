/*
 * Copyright 2009 LinkedIn, Inc.
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

package voldemort.utils.impl;

import static voldemort.utils.impl.CommandLineParameterizer.DESTINATION_DIRECTORY_PARAM;
import static voldemort.utils.impl.CommandLineParameterizer.HOST_NAME_PARAM;
import static voldemort.utils.impl.CommandLineParameterizer.HOST_USER_ID_PARAM;
import static voldemort.utils.impl.CommandLineParameterizer.SOURCE_DIRECTORY_PARAM;
import static voldemort.utils.impl.CommandLineParameterizer.SSH_PRIVATE_KEY_PARAM;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;

import voldemort.utils.Deployer;
import voldemort.utils.RemoteOperationException;

/**
 * RsyncDeployer is an implementation of Deployer that uses the rsync command
 * line utility to copy the data to the remote host. It's a little bit clunky
 * going out to the shell to do this, but rsync is very flexible and can ride
 * over SSH.
 * 
 */

public class RsyncDeployer extends CommandLineRemoteOperation implements Deployer {

    private final Collection<String> hostNames;

    private final File sshPrivateKey;

    private final String hostUserId;

    private final File sourceDirectory;

    private final String destinationDirectory;

    /**
     * Creates a new RsyncDeployer instance.
     * 
     * @param hostNames External host names to which to deploy the Voldemort
     *        distribution
     * @param sshPrivateKey SSH private key file on local filesystem that can
     *        access all of the remote hosts, or null if not needed
     * @param sourceDirectory Directory of Voldemort distribution on local file
     *        system
     * @param hostUserId User ID on the remote hosts; assumed to be the same for
     *        all of the remote hosts
     * @param destinationDirectory Parent directory into which the
     *        sourceDirectory will be copied, relative to the home directory of
     *        the user on the remote system represented by hostUserId; e.g. if
     *        you want to deploy Voldemort to /root/somedirectory/voldemort, the
     *        destinationDirectory would be simply "somedirectory"; assumed to
     *        be the same for all of the remote hosts
     */

    public RsyncDeployer(Collection<String> hostNames,
                         File sshPrivateKey,
                         String hostUserId,
                         File sourceDirectory,
                         String destinationDirectory) {
        this.hostNames = hostNames;
        this.sshPrivateKey = sshPrivateKey;
        this.hostUserId = hostUserId;
        this.sourceDirectory = sourceDirectory;
        this.destinationDirectory = destinationDirectory;
    }

    public void execute() throws RemoteOperationException {
        if(logger.isInfoEnabled())
            logger.info("Rsync-ing " + sourceDirectory.getAbsolutePath() + " to "
                        + destinationDirectory + " on remote hosts: " + hostNames);

        if(!sourceDirectory.exists())
            throw new RemoteOperationException(sourceDirectory.getAbsolutePath()
                                               + " does not exist");

        if(!sourceDirectory.isDirectory())
            throw new RemoteOperationException("Directory " + sourceDirectory.getAbsolutePath()
                                               + " is not a directory");

        CommandLineParameterizer commandLineParameterizer = new CommandLineParameterizer("RsyncDeployer.rsync"
                                                                                         + (sshPrivateKey != null ? ""
                                                                                                                 : ".nokey"));
        Map<String, String> hostNameCommandLineMap = new HashMap<String, String>();

        for(String hostName: hostNames) {
            Map<String, String> parameters = new HashMap<String, String>();
            parameters.put(HOST_NAME_PARAM, hostName);
            parameters.put(HOST_USER_ID_PARAM, hostUserId);
            parameters.put(SSH_PRIVATE_KEY_PARAM,
                           sshPrivateKey != null ? sshPrivateKey.getAbsolutePath() : null);
            parameters.put(DESTINATION_DIRECTORY_PARAM, destinationDirectory);
            parameters.put(SOURCE_DIRECTORY_PARAM, sourceDirectory.getAbsolutePath());

            hostNameCommandLineMap.put(hostName, commandLineParameterizer.parameterize(parameters));
        }

        execute(hostNameCommandLineMap);

        if(logger.isInfoEnabled())
            logger.info("Rsync-ing complete");
    }

    @Override
    protected Callable<?> getCallable(UnixCommand command) {
        // Note that we pass in "false" here because we don't want to log
        // uploads of "VoldemortException" as warnings. (We can trip up the
        // exception detection easily.)
        CommandOutputListener commandOutputListener = new LoggingCommandOutputListener(null,
                                                                                       logger,
                                                                                       false);
        return new ExitCodeCallable(command, commandOutputListener);
    }

}
