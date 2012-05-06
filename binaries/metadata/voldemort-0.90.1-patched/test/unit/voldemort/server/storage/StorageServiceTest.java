package voldemort.server.storage;

import java.io.File;
import java.util.List;

import junit.framework.TestCase;
import voldemort.MockTime;
import voldemort.ServerTestUtils;
import voldemort.TestUtils;
import voldemort.cluster.Cluster;
import voldemort.server.StoreRepository;
import voldemort.server.VoldemortConfig;
import voldemort.server.scheduler.SchedulerService;
import voldemort.store.StoreDefinition;
import voldemort.store.metadata.MetadataStore;

/**
 * Test that the storage service is able to load all stores.
 * 
 * 
 */
public class StorageServiceTest extends TestCase {

    private Cluster cluster;
    private StoreRepository storeRepository;
    private StorageService storage;
    private SchedulerService scheduler;
    private List<StoreDefinition> storeDefs;

    @Override
    public void setUp() {
        File temp = TestUtils.createTempDir();
        VoldemortConfig config = new VoldemortConfig(0, temp.getAbsolutePath());
        new File(config.getMetadataDirectory()).mkdir();
        config.setBdbCacheSize(100000);
        this.scheduler = new SchedulerService(1, new MockTime());
        this.cluster = ServerTestUtils.getLocalCluster(1);
        this.storeDefs = ServerTestUtils.getStoreDefs(2);
        this.storeRepository = new StoreRepository();
        MetadataStore mdStore = ServerTestUtils.createMetadataStore(cluster, storeDefs);
        storage = new StorageService(storeRepository, mdStore, scheduler, config);
        storage.start();
    }

    public void testStores() {
        StoreRepository repo = storage.getStoreRepository();
        for(StoreDefinition def: storeDefs) {
            // test local stores
            assertTrue("Missing local store '" + def.getName() + "'.",
                       repo.hasLocalStore(def.getName()));
            assertEquals(def.getName(), repo.getLocalStore(def.getName()).getName());

            assertTrue("Missing storage engine '" + def.getName() + "'.",
                       repo.hasStorageEngine(def.getName()));
            assertEquals(def.getName(), repo.getStorageEngine(def.getName()).getName());

            for(int node = 0; node < cluster.getNumberOfNodes(); node++) {
                assertTrue("Missing node store '" + def.getName() + "'.",
                           repo.hasNodeStore(def.getName(), node));
                assertEquals(def.getName(), repo.getNodeStore(def.getName(), node).getName());
            }
        }
    }
}
