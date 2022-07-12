package com.sap.ic.cmh.objectstore.service;

import com.sap.ic.cmh.objectstore.config.AzureStorageConfiguration;
import com.sap.ic.cmh.objectstore.repository.ObjectStoreRepository;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;
import org.jclouds.blobstore.BlobStoreContext;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.annotation.Autowired;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class AzureObjectStoreServiceTest {
    @InjectMocks
    @Autowired
    AzureObjectStoreService service;

    @Mock
    BlobStoreContext context;

    @Mock
    ObjectStoreUtil objectStoreUtil;

    @Before
    public void beforeClass(){
        MockitoAnnotations.openMocks(this);

    }

    @Test(expected = Exception.class)
    public void uploadFileTest(){
        AzureStorageConfiguration azureConfig = Mockito.mock(AzureStorageConfiguration.class);
        when(azureConfig.getBlobStoreContext(anyString())).thenReturn(context);
        ObjectStoreRepository repository = Mockito.mock(ObjectStoreRepository.class);
        doNothing().when(repository).setContext(any());
        service = new AzureObjectStoreService(azureConfig,repository);
        service.uploadFile("subdomain",new byte[9],"test","mintype");
    }

}