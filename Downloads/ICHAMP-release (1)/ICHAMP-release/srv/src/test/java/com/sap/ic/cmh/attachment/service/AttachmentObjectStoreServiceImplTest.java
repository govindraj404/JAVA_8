package com.sap.ic.cmh.attachment.service;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.InputStream;

import org.jclouds.blobstore.BlobStoreContext;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.objectstore.config.AzureStorageConfiguration;
import com.sap.ic.cmh.objectstore.model.UploadStatus;
import com.sap.ic.cmh.objectstore.repository.ObjectStoreRepository;
import com.sap.ic.cmh.objectstore.service.AzureObjectStoreService;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;
import com.sap.ic.cmh.utils.PlatformUtil;

public class AttachmentObjectStoreServiceImplTest {

	@InjectMocks
	@Autowired
	AttachmentObjectStoreServiceImpl attachmentObjectStoreServiceImpl;
	@Mock
	ObjectStoreUtil objectStoreUtil;
	@Mock
	ObjectStoreRepository repository;
	@Mock
	BlobStoreContext context;
	@Mock
	AzureStorageConfiguration azureConfig;
	@Mock
	InputStream inputStream;

	@Mock
	AzureObjectStoreService azureObjectStoreService;
	@Mock
	PlatformUtil platformUtil;
	@Mock
	DestinationService destinationService;
	@Mock
	CdsRuntime cdsRuntime;

	String fileName = "file.txt";
	String bucketName = "bucketName";
	String contentType = "multiPartFile";
	String subdomain = "subdomain";

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
       this.objectStoreUtil = mock(ObjectStoreUtil.class);
	}

	@Test
	public void testUploadFile() {
		BlobStoreContext blobStoreContext = azureConfig.getBlobStoreContext(subdomain);
		when(azureConfig.getBlobStoreContext(subdomain)).thenReturn(blobStoreContext);
		repository.setContext(blobStoreContext);
		byte[] bytes = new byte[10];
		attachmentObjectStoreServiceImpl.uploadFile(bucketName, bytes, fileName, contentType);
	}

	@Test
	public void testDeleteFile() {
		BlobStoreContext blobStoreContext = azureConfig.getBlobStoreContext(subdomain);
		when(azureConfig.getBlobStoreContext(subdomain)).thenReturn(blobStoreContext);
		repository.setContext(blobStoreContext);
		when(objectStoreUtil.getBucket(subdomain)).thenReturn(bucketName);
		attachmentObjectStoreServiceImpl.deleteFile(subdomain, fileName);
	}

	@Test
	public void testGetFile() {
		BlobStoreContext blobStoreContext = azureConfig.getBlobStoreContext(subdomain);
		when(azureConfig.getBlobStoreContext(subdomain)).thenReturn(blobStoreContext);
		repository.setContext(blobStoreContext);
		when(objectStoreUtil.getBucket(subdomain)).thenReturn(bucketName);
		attachmentObjectStoreServiceImpl.getFile("subdomain", "file.txt");
	}
	@Test
	public void testListObjects() {
		BlobStoreContext blobStoreContext = azureConfig.getBlobStoreContext(subdomain);
		when(azureConfig.getBlobStoreContext(subdomain)).thenReturn(blobStoreContext);
		repository.setContext(blobStoreContext);
		when(objectStoreUtil.getBucket(subdomain)).thenReturn(bucketName);
		attachmentObjectStoreServiceImpl.listObjects(subdomain);
	}
	@Test
	public void testListFileObjects() {
		BlobStoreContext blobStoreContext = azureConfig.getBlobStoreContext(subdomain);
		when(azureConfig.getBlobStoreContext(subdomain)).thenReturn(blobStoreContext);
		repository.setContext(blobStoreContext);
		when(objectStoreUtil.getBucket(subdomain)).thenReturn(bucketName);
		attachmentObjectStoreServiceImpl.listFileObjects(subdomain, fileName);
	}

	@Test
	public void testIsBlobExist() {
		BlobStoreContext blobStoreContext = azureConfig.getBlobStoreContext(subdomain);
		when(azureConfig.getBlobStoreContext(subdomain)).thenReturn(blobStoreContext);
		repository.setContext(blobStoreContext);
		when(objectStoreUtil.getBucket(subdomain)).thenReturn(bucketName);
		attachmentObjectStoreServiceImpl.isBlobExist(subdomain, fileName);
	}

}
