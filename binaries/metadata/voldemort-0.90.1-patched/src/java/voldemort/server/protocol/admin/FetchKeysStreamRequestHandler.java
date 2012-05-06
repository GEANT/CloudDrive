package voldemort.server.protocol.admin;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import voldemort.client.protocol.pb.ProtoUtils;
import voldemort.client.protocol.pb.VAdminProto;
import voldemort.client.protocol.pb.VAdminProto.FetchPartitionEntriesRequest;
import voldemort.server.StoreRepository;
import voldemort.server.VoldemortConfig;
import voldemort.store.ErrorCodeMapper;
import voldemort.store.metadata.MetadataStore;
import voldemort.store.stats.StreamStats;
import voldemort.store.stats.StreamStats.Operation;
import voldemort.utils.ByteArray;
import voldemort.utils.NetworkClassLoader;
import voldemort.utils.RebalanceUtils;

import com.google.protobuf.Message;

public class FetchKeysStreamRequestHandler extends FetchStreamRequestHandler {

    public FetchKeysStreamRequestHandler(FetchPartitionEntriesRequest request,
                                         MetadataStore metadataStore,
                                         ErrorCodeMapper errorCodeMapper,
                                         VoldemortConfig voldemortConfig,
                                         StoreRepository storeRepository,
                                         NetworkClassLoader networkClassLoader,
                                         StreamStats stats) {
        super(request,
              metadataStore,
              errorCodeMapper,
              voldemortConfig,
              storeRepository,
              networkClassLoader,
              stats,
              Operation.FETCH_KEYS);
    }

    public StreamRequestHandlerState handleRequest(DataInputStream inputStream,
                                                   DataOutputStream outputStream)
            throws IOException {
        if(!keyIterator.hasNext())
            return StreamRequestHandlerState.COMPLETE;

        long startNs = System.nanoTime();
        ByteArray key = keyIterator.next();
        stats.recordDiskTime(handle, System.nanoTime() - startNs);

        throttler.maybeThrottle(key.length());
        if(RebalanceUtils.checkKeyBelongsToPartition(nodeId,
                                                     key.get(),
                                                     replicaToPartitionList,
                                                     initialCluster,
                                                     storeDef)
           && filter.accept(key, null) && counter % skipRecords == 0) {
            VAdminProto.FetchPartitionEntriesResponse.Builder response = VAdminProto.FetchPartitionEntriesResponse.newBuilder();
            response.setKey(ProtoUtils.encodeBytes(key));

            fetched++;
            handle.incrementEntriesScanned();
            Message message = response.build();

            startNs = System.nanoTime();
            ProtoUtils.writeMessage(outputStream, message);
            stats.recordNetworkTime(handle, System.nanoTime() - startNs);
        }

        // log progress
        counter++;

        if(0 == counter % 100000) {
            long totalTime = (System.currentTimeMillis() - startTime) / 1000;

            logger.info("Fetch keys scanned " + counter + " keys, fetched " + fetched
                        + " keys for store '" + storageEngine.getName()
                        + "' replicaToPartitionList:" + replicaToPartitionList + " in " + totalTime
                        + " s");
        }

        if(keyIterator.hasNext())
            return StreamRequestHandlerState.WRITING;
        else {
            stats.closeHandle(handle);
            return StreamRequestHandlerState.COMPLETE;
        }
    }
}
