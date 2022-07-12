package com.sap.ic.cmh.attachment.controller;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;

import com.sap.ic.cmh.attachment.service.AttachmentObjectStoreService;
import com.sap.ic.cmh.customercomplaint.service.AttachmentOperationsService;
import com.sap.ic.cmh.customercomplaint.validation.AttachmentsValidation;
import com.sap.ic.cmh.objectstore.model.UploadStatus;
import com.sap.ic.cmh.utils.XsuaaToken;


public class AttachmentControllerTest {

	@InjectMocks
	@Autowired
	AttachmentController attachmentController;
	@Mock
	AttachmentObjectStoreService attachmentObjectStoreService;
	@Mock
	AttachmentOperationsService attachmentOperationsService;
	@Mock
	AttachmentsValidation attachmentsValidation;
	@Mock
	XsuaaToken fileAuthorizationTokenPojo;
	UploadStatus uploadFile;
    String subdomain = "subdomain";
    
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		uploadFile = new UploadStatus();
		uploadFile.setStatus("SUCCESS");
		
	}
	
	@Test
	public void testUploadEmptyFile() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		byte[] rateExceptionsFile = new byte[0];
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file",
	            "OriginalName.png",
	            "image/png",
	            rateExceptionsFile);
		attachmentController.upload(mockMultipartFile, "111", "OriginalName.png");
	}
	
	@Test
	public void testUploadFile() {
		
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		byte[] rateExceptionsFile = new byte[2048];
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file",
	            "OriginalName.png",
	            "image/png",
	            rateExceptionsFile);
		String size = String.valueOf(mockMultipartFile.getSize());
		
		when(attachmentOperationsService.checkForMalware(rateExceptionsFile)).thenReturn("false");
		when(attachmentsValidation.validateAttachments(mockMultipartFile, size)).thenReturn("");
		when(attachmentObjectStoreService.uploadFile(subdomain, rateExceptionsFile, "OriginalName.png", "image/png")).thenReturn(uploadFile);
		attachmentController.upload(mockMultipartFile, "111", "OriginalName.png");
	}
	
	@Test
	public void testUploadFileExp() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		byte[] rateExceptionsFile = new byte[2048];
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file",
	            "OriginalName.png",
	            "image/png",
	            rateExceptionsFile);
		String size = String.valueOf(mockMultipartFile.getSize());
		
		when(attachmentOperationsService.checkForMalware(rateExceptionsFile)).thenReturn("false");
		when(attachmentsValidation.validateAttachments(mockMultipartFile, size)).thenReturn("");
		when(attachmentObjectStoreService.uploadFile(subdomain, rateExceptionsFile, "OriginalName.png", "image/png")).thenReturn(uploadFile);
		doThrow(new IllegalArgumentException()).when(attachmentOperationsService).storeAttachmentInHanaDb("OriginalName.png", "111", size, "image/png", "1");
		attachmentController.upload(mockMultipartFile, "111", "OriginalName.png");
	}
	
	@Test
	public void testUploadFileScanTrue() {
		UploadStatus uploadFile = new UploadStatus();
		uploadFile.setStatus("SUCCESS");
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		byte[] rateExceptionsFile = new byte[2048];
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file",
	            "OriginalName.png",
	            "image/png",
	            rateExceptionsFile);
		when(attachmentOperationsService.checkForMalware(rateExceptionsFile)).thenReturn("true");
		attachmentController.upload(mockMultipartFile, "111", "OriginalName.png");
	}
	
	@Test
	public void testUploadFileValidationError() {
		UploadStatus uploadFile = new UploadStatus();
		uploadFile.setStatus("SUCCESS");
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		byte[] rateExceptionsFile = new byte[2048];
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file",
	            "OriginalName.png",
	            "image/png",
	            rateExceptionsFile);
		String size = String.valueOf(mockMultipartFile.getSize());
		
		when(attachmentOperationsService.checkForMalware(rateExceptionsFile)).thenReturn("false");
		when(attachmentsValidation.validateAttachments(mockMultipartFile, size)).thenReturn("error message");
		attachmentController.upload(mockMultipartFile, "111", "OriginalName");
	}
	
	@Test
	public void testListFiles() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		attachmentController.listFiles();
		
	}
	
	@Test
	public void testListFilesExp() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentObjectStoreService.listObjects(subdomain)).thenThrow(new IllegalArgumentException());
		attachmentController.listFiles();
	}
	
	@Test
	public void testRenameAttachment() {
		when(attachmentOperationsService.renameAttachment("NewName", "111")).thenReturn("Success");
		attachmentController.renameAttachment("NewName", "111");
	}
	@Test
	public void testGetFiles() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		attachmentController.getFiles("OriginalName");
	}
	@Test
	public void testGetFilesExp() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentObjectStoreService.listFileObjects(subdomain, "OriginalName")).thenThrow(new IllegalArgumentException());
		attachmentController.getFiles("OriginalName");
	}
	@Test
	public void testGetDetailsAndDownloadFile() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentsValidation.validateAttachmentExists("1111")).thenReturn("Name");
		when(attachmentObjectStoreService.isBlobExist(subdomain, "Name")).thenReturn(true);
		attachmentController.getDetailsAndDownloadFile("1111");
	}
	
	@Test
	public void testGetDetailsAndDownloadFileFileValidationError() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentsValidation.validateAttachmentExists("1111")).thenReturn("");
		when(attachmentObjectStoreService.isBlobExist(subdomain, "Name")).thenReturn(true);
		attachmentController.getDetailsAndDownloadFile("1111");
	}
	@Test
	public void testGetDetailsAndDownloadFileFileNotExists() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentsValidation.validateAttachmentExists("1111")).thenReturn("Name");
		when(attachmentObjectStoreService.isBlobExist(subdomain, "Name")).thenReturn(false);
		attachmentController.getDetailsAndDownloadFile("1111");
	}
	
	
	@Test
	public void testDeleteAttachment() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentObjectStoreService.isBlobExist(subdomain, "1111")).thenReturn(true);
		when(attachmentObjectStoreService.deleteFile(subdomain, "1111")).thenReturn(true);
		attachmentController.deleteAttachment("1111");
		
	}
	
	@Test
	public void testDeleteAttachmentBlobExistsFalse() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentObjectStoreService.isBlobExist("subdomain", "1111")).thenReturn(false);
		attachmentController.deleteAttachment("1111");
	}
	
	@Test
	public void testDeleteAttachmentDeleteFileFalse() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		when(attachmentObjectStoreService.isBlobExist("subdomain", "1111")).thenReturn(true);
		when(attachmentObjectStoreService.deleteFile("subdomain", "1111")).thenReturn(false);
		attachmentController.deleteAttachment("1111");
	}
	
	@Test
	public void testDeleteAttachmentFileNull() {
		when(fileAuthorizationTokenPojo.getSubdomain()).thenReturn("subdomain");
		attachmentController.deleteAttachment(null);
	}

}
