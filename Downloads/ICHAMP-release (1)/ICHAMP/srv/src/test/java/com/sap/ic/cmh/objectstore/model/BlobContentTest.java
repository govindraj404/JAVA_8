package com.sap.ic.cmh.objectstore.model;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class BlobContentTest {
	
	@InjectMocks
	BlobContent blobContent;
	
	 @Before
	 public void beforeClass() {
	   MockitoAnnotations.openMocks(this);
	 }
	 
	 @Test
	 public void getMethodTest() {
		 blobContent.getBucket();
		 blobContent.getContentType();
		 blobContent.getEtag();
		 blobContent.getName();
		 blobContent.getSize();
		 blobContent.getUrl();
		 blobContent.getUserMetadata();
	 }

}
