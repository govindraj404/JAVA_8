package com.sap.ic.cmh.customercomplaint.validation;

import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;

import com.sap.cds.Struct;
import com.sap.ic.cmh.customercomplaint.service.AttachmentOperationsService;
import com.sap.ic.cmh.utils.LocaleMessageHelper;

import cds.gen.customercomplaintservice.Attachments;

public class AttachmentsValidationImplTest {
	
	@InjectMocks
    @Autowired
	AttachmentsValidationImpl attachmentsValidationImpl;
	@Mock
	LocaleMessageHelper localeMessageHelper;
	@Mock
    AttachmentOperationsService attachmentOperationsService;
	
Attachments attachments;
	
	@Before
	public void beforeClass() {
	  MockitoAnnotations.openMocks(this);
	  attachments = Struct.create(Attachments.class);
	  attachments.setId("1");
	  attachments.setName("Test.png");
	  attachments.setType("image/png");
	  attachments.setSize("1024");
	}
	
	@Test
	public void testValidateAttachmentsSizeError() {
		byte[] file1 = "Hello, World!".getBytes();
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file", "OriginalName.txt", "text/plain", file1);
		attachmentsValidationImpl.validateAttachments(mockMultipartFile, "5242890");
	}
	
	@Test
	public void testValidateAttachmentsMimeTypeError() {
		byte[] file1 = "Hello, World!".getBytes();
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file", "OriginalName.txt", "text/plain", file1);
		attachmentsValidationImpl.validateAttachments(mockMultipartFile, "178893");
	}
	
	@Test
	public void testFileType() {
		byte[] file1 = "Hello, World!".getBytes();
		MockMultipartFile mockMultipartFile = new MockMultipartFile("file", "OriginalName.png", "image/png", file1);
		attachmentsValidationImpl.validateAttachments(mockMultipartFile, "178893");
	}
	
	@Test
	public void testValidateAttachmentExists() {
		when(attachmentOperationsService.getAttachmentDetails(attachments.getId())).thenReturn(attachments);
		attachmentsValidationImpl.validateAttachmentExists(attachments.getId());
	}
	
	@Test
	public void testValidateAttachmentExistsNull() {
		when(attachmentOperationsService.getAttachmentDetails(attachments.getId())).thenReturn(null);
		attachmentsValidationImpl.validateAttachmentExists(attachments.getId());
	}
	
	
	
	

}
