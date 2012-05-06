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

package voldemort.xml;


import java.io.StringReader;
import java.util.List;

import junit.framework.TestCase;
import voldemort.VoldemortTestConstants;
import voldemort.store.StoreDefinition;

public class StoreDefinitionMapperTest extends TestCase {

    public void testParsing() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        List<StoreDefinition> storeDefs = mapper.readStoreList(new StringReader(VoldemortTestConstants.getSimpleStoreDefinitionsXml()));
        String output = mapper.writeStoreList(storeDefs);
        List<StoreDefinition> found = mapper.readStoreList(new StringReader(output));
        checkEquals(storeDefs, found);
        for(StoreDefinition def: storeDefs) {
            String xml = mapper.writeStore(def);
            StoreDefinition newDef = mapper.readStore(new StringReader(xml));
            assertEquals(def, newDef);
        }
    }

    public void testSingleStore() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        List<StoreDefinition> storeDefs = mapper.readStoreList(new StringReader(VoldemortTestConstants.getSingleStoreDefinitionsXml()));
        String written = mapper.writeStoreList(storeDefs);
        assertEquals(storeDefs, mapper.readStoreList(new StringReader(written)));
    }

    public void testNoVersionStore() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        List<StoreDefinition> storeDefs = mapper.readStoreList(new StringReader(VoldemortTestConstants.getNoVersionStoreDefinitionsXml()));
        String written = mapper.writeStoreList(storeDefs);
        assertEquals(storeDefs, mapper.readStoreList(new StringReader(written)));
    }

    public void testSingleStoreWithZones() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        List<StoreDefinition> storeDefs = mapper.readStoreList(new StringReader(VoldemortTestConstants.getSingleStoreWithZonesXml()));
        String written = mapper.writeStoreList(storeDefs);
        assertEquals(storeDefs, mapper.readStoreList(new StringReader(written)));
    }

    public void testTwoVersionsOnKeyFails() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        try {
            mapper.readStoreList(new StringReader(VoldemortTestConstants.getStoreWithTwoKeyVersions()));
            fail("There are multiple versions on the key serializer, which should not be allowed.");
        } catch(MappingException e) {
            // this is good
        }
    }

    public void testRetentionStore() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        List<StoreDefinition> storeDefs = mapper.readStoreList(new StringReader(VoldemortTestConstants.getStoreDefinitionsWithRetentionXml()));
        String written = mapper.writeStoreList(storeDefs);
        assertEquals(storeDefs, mapper.readStoreList(new StringReader(written)));
    }

    public void testCompressedStore() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        List<StoreDefinition> storeDefs = mapper.readStoreList(new StringReader(VoldemortTestConstants.getCompressedStoreDefinitionsXml()));
        String written = mapper.writeStoreList(storeDefs);
        assertEquals(storeDefs, mapper.readStoreList(new StringReader(written)));
    }

    public void testView() {
        StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
        List<StoreDefinition> storeDefs = mapper.readStoreList(new StringReader(VoldemortTestConstants.getViewStoreDefinitionXml()));
        String written = mapper.writeStoreList(storeDefs);
        checkEquals(storeDefs, mapper.readStoreList(new StringReader(written)));
    }

    private void checkEquals(List<StoreDefinition> l1, List<StoreDefinition> l2) {
        assertEquals(l1.size(), l2.size());
        for(int i = 0; i < l1.size(); i++)
            assertEquals(l1.get(i), l2.get(i));
    }

}
