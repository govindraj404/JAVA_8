package com.sap.ic.cmh.customercomplaint.handler;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.draft.DraftCancelEventContext;
import com.sap.cds.services.draft.DraftNewEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.attachment.controller.AttachmentController;
import com.sap.ic.cmh.customercomplaint.service.AttachmentOperationsService;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;
import com.sap.ic.cmh.utils.CqnAnalyzerUtil;

import cds.gen.customercomplaintservice.Attachments;
import cds.gen.customercomplaintservice.CustomerComplaints;

public class CustomerComplaintHandlerTest {
	
	@InjectMocks
	@Autowired
	CustomerComplaintHandler customerComplaintHandler;
	
	@Mock
	Messages messages;
	@Mock
    AttachmentOperationsService attachmentOperationsService;
	@Mock
    ObjectStoreUtil objectStoreUtil;
	@Mock
    AttachmentController attachmentController;
	@Mock
	DraftNewEventContext context;
	@Mock
	DraftCancelEventContext draftCancelEventContext;
	@Mock
    CqnAnalyzerUtil cqnAnalyzerUtil;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;
	
	CustomerComplaints customerComplaint;
	Attachments attachments;
	Attachments activeAttachments;
	Attachments activeAttachments1;
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		customerComplaint = Struct.create(CustomerComplaints.class);
		customerComplaint.setId("111");
		customerComplaint.setComplaintCategoryCode("CUSCUMP");
		customerComplaint.setIndividualComplaint(true);
		
		attachments = Struct.create(Attachments.class);
		attachments.setId("1234");
		attachments.setParentID("111");
		
		activeAttachments = Struct.create(Attachments.class);
		activeAttachments.setId("1234");
		activeAttachments.setParentID("111");
		
		activeAttachments1 = Struct.create(Attachments.class);
		activeAttachments1.setId("5678");
		activeAttachments1.setParentID("111");
		
		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}
	
	@Test
	public void testBeforeComplaintDraftCreation() {
		customerComplaintHandler.beforeComplaintDraftCreation(context, customerComplaint);
	}
	@Test
	public void testBeforeComplaintDraftCreationFalse() {
		customerComplaint.setIndividualComplaint(false);
		customerComplaintHandler.beforeComplaintDraftCreation(context, customerComplaint);
	}
	@Test
	public void testBeforeComplaintDraftCreationNull() {
		customerComplaint.setIndividualComplaint(null);
		customerComplaintHandler.beforeComplaintDraftCreation(context, customerComplaint);
	}
	@Test
	public void testOnCustomerComplaintsDelete() {
		List<Attachments> attachmentList = new ArrayList<>();
		attachmentList.add(attachments);
		 Map<String, Object> targetKeys = new HashMap<>();
		 targetKeys.put("ID", "111");
        when(cqnAnalyzerUtil.provideTargetKeysDraft(draftCancelEventContext)).thenReturn(targetKeys);
        when(attachmentOperationsService.getDraftAttachmentDetailsBasedOnComplaintId(attachments.getParentID()))
        .thenReturn(attachmentList);
        customerComplaintHandler.onCustomerComplaintsDelete(draftCancelEventContext);
	}
	@Test
	public void testOnCustomerComplaintsDeleteEmptyList() {
		List<Attachments> attachmentList = new ArrayList<>();
		Map<String, Object> targetKeys = new HashMap<>();
		 targetKeys.put("ID", "111");
       when(cqnAnalyzerUtil.provideTargetKeysDraft(draftCancelEventContext)).thenReturn(targetKeys);
        when(attachmentOperationsService.getDraftAttachmentDetailsBasedOnComplaintId(attachments.getParentID()))
        .thenReturn(attachmentList);
        customerComplaintHandler.onCustomerComplaintsDelete(draftCancelEventContext);
	}
	@Test
	public void testOnCustomerComplaintsUpdate() {
		List<Attachments> draftAttachmentList = new ArrayList<>();
		draftAttachmentList.add(attachments);
		List<Attachments> activeAttachmentList = new ArrayList<>();
		activeAttachmentList.add(activeAttachments);
		activeAttachmentList.add(activeAttachments1);
		 when(attachmentOperationsService.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID()))
	        .thenReturn(draftAttachmentList);
		 when(attachmentOperationsService.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID()))
	        .thenReturn(activeAttachmentList);
		 customerComplaintHandler.onCustomerComplaintsUpdate(customerComplaint);
	}
	@Test
	public void testOnCustomerComplaintsUpdateDraftListEmpty() {
		List<Attachments> draftAttachmentList = new ArrayList<>();
		List<Attachments> activeAttachmentList = new ArrayList<>();
		activeAttachmentList.add(activeAttachments);
		activeAttachmentList.add(activeAttachments1);
		 when(attachmentOperationsService.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID()))
	        .thenReturn(draftAttachmentList);
		 when(attachmentOperationsService.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID()))
	        .thenReturn(activeAttachmentList);
		 customerComplaintHandler.onCustomerComplaintsUpdate(customerComplaint);
	}
	@Test
	public void testOnCustomerComplaintsUpdateActiveListEmpty() {
		List<Attachments> draftAttachmentList = new ArrayList<>();
		draftAttachmentList.add(attachments);
		List<Attachments> activeAttachmentList = new ArrayList<>();
		 when(attachmentOperationsService.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID()))
	        .thenReturn(draftAttachmentList);
		 when(attachmentOperationsService.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID()))
	        .thenReturn(activeAttachmentList);
		 customerComplaintHandler.onCustomerComplaintsUpdate(customerComplaint);
	}
	@Test
	public void testOnCustomerComplaintsUpdateDiffListEmpty() {
		List<Attachments> draftAttachmentList = new ArrayList<>();
		draftAttachmentList.add(attachments);
		List<Attachments> activeAttachmentList = new ArrayList<>();
		activeAttachmentList.add(activeAttachments);
		 when(attachmentOperationsService.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID()))
	        .thenReturn(draftAttachmentList);
		 when(attachmentOperationsService.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID()))
	        .thenReturn(activeAttachmentList);
		 customerComplaintHandler.onCustomerComplaintsUpdate(customerComplaint);
	}

}
