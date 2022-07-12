package com.sap.ic.cmh.objectstore.repository;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.jclouds.blobstore.BlobStore;
import org.jclouds.blobstore.BlobStoreContext;
import org.jclouds.blobstore.ContainerNotFoundException;
import org.jclouds.blobstore.domain.Blob;
import org.jclouds.blobstore.domain.BlobBuilder;
import org.jclouds.blobstore.domain.PageSet;
import org.jclouds.blobstore.domain.StorageMetadata;
import org.jclouds.io.Payload;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class ObjectStoreRepositoryTest {
    @InjectMocks
    ObjectStoreRepository repo;
    @Mock
     BlobStoreContext context;
    @Mock
     BlobStore blobStore;
    @Mock
    BlobBuilder.PayloadBlobBuilder  blobBuilder;
    @Mock
    Blob plPayload;
    @Mock
    Blob blob;
    @Mock
    InputStream stream;
    @Mock
    Payload payload;
    PageSet<? extends StorageMetadata> value;
    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void uploadFileTest(){
        byte[] byteArray=new byte[10];
        Map<String,String> map=new HashMap<>();
        when(blobStore.blobBuilder(any())).thenReturn(blobBuilder);
        when(blobBuilder.payload(any(Payload.class))).thenReturn(blobBuilder);
        when(blobBuilder.contentType(any(String.class))).thenReturn(blobBuilder);
        when(blobBuilder.expires(any(Date.class))).thenReturn(blobBuilder);
        when(blobBuilder.userMetadata(any(Map.class))).thenReturn(blobBuilder);
        when(blobBuilder.build()).thenReturn(plPayload);
        when(context.getBlobStore()).thenReturn(blobStore);
        repo.uploadFile("test",byteArray,"test","MIM");

    }

    @Test
    public void uploadFileExpTest(){
        byte[] byteArray=new byte[10];
        when(blobStore.blobBuilder(any())).thenReturn(blobBuilder);
        when(blobBuilder.payload(any(Payload.class))).thenReturn(blobBuilder);
        when(blobBuilder.contentType(any(String.class))).thenReturn(blobBuilder);
        when(blobBuilder.expires(any(Date.class))).thenReturn(blobBuilder);
        when(blobBuilder.userMetadata(any(Map.class))).thenReturn(blobBuilder);
        when(blobBuilder.build()).thenReturn(null);
        when(context.getBlobStore()).thenThrow(ContainerNotFoundException.class);
        repo.uploadFile(null,byteArray,null,null);

    }

    @Test
    public void downloadFileTest(){
         when(context.getBlobStore()).thenReturn(blobStore);
        when(blobStore.getBlob(any(),any())).thenReturn(blob);
        when(blob.getPayload()).thenReturn(payload);
        repo.downloadFile("test","text");
    }
  
    @Test//( expected = Exception.class)
    public void downloadFileExpTest() throws IOException{
        when(context.getBlobStore()).thenReturn(blobStore);
        when(blobStore.getBlob(any(),any())).thenReturn(blob);
        when(blob.getPayload()).thenReturn(payload);
        when(payload.openStream()).thenThrow(new IOException());
        //doThrow(new IOException()).when(payload);
        repo.downloadFile("test","text");
    }

    @Test
    public void isBlobExistTest(){
        when(context.getBlobStore()).thenReturn(blobStore);
        when(blobStore.blobExists(any(),any())).thenReturn(true);
        when(blob.getPayload()).thenReturn(null);
        repo.isBlobExist("test","text");
    }
    @Test
    public void setGetContext(){
        repo.setContext(context);
        repo.getContext();
    }
    
	@Test
	public void testDeleteFile() {
		// setup
		String bucketName = "bucketName";
		String filename = "fileName";
		ObjectStoreRepository objectStoreRepositorySpy = spy(ObjectStoreRepository.class);
		doReturn(context).when(objectStoreRepositorySpy).getContext();
		when(context.getBlobStore()).thenReturn(blobStore);
		// execute
		objectStoreRepositorySpy.deleteFile(bucketName, filename);
	}
	
	@Test
	public void testListFiles() {
		String bucketName ="bucketName";
		ObjectStoreRepository objectStoreRepositorySpy = spy(ObjectStoreRepository.class);
		doReturn(context).when(objectStoreRepositorySpy).getContext();
		when(context.getBlobStore()).thenReturn(blobStore);
		 value = blobStore.list();
		doReturn(value).when(blobStore).list(bucketName);
		objectStoreRepositorySpy.listFiles(bucketName);
	}
	
	@Test
	public void testListNamefiles() {
		String bucketName ="bucketName";
		String filename="fileName";
		ObjectStoreRepository objectStoreRepositorySpy = spy(ObjectStoreRepository.class);
		doReturn(context).when(objectStoreRepositorySpy).getContext();
		when(context.getBlobStore()).thenReturn(blobStore);
		objectStoreRepositorySpy.listNameFiles(bucketName,filename);
	}
}
