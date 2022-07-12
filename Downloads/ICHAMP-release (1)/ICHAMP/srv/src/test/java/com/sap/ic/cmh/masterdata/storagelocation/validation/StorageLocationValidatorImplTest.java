package com.sap.ic.cmh.masterdata.storagelocation.validation;

import cds.gen.masterdataservice.StorageLocations;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;

public class StorageLocationValidatorImplTest {

    @InjectMocks
    @Autowired
    StorageLocationValidatorImpl storageLocationValidatorImpl;

    @Spy
    private DataValidator dataValidator;

    @Mock
    private Messages messages;

    private StorageLocations storageLocations;

    @Before
    public void beforeClass() throws Exception {
        MockitoAnnotations.openMocks(this);
        storageLocations = Struct.create(StorageLocations.class);
        storageLocations.setStorageLocation("0002");
        storageLocations.setStorageLocationName("Production");
        storageLocations.setPlantCode("BP11");
    }

    @Test
    public void testCheckInputsSanitized(){
        storageLocationValidatorImpl.checkInputsSanitized(storageLocations);
    }
}
