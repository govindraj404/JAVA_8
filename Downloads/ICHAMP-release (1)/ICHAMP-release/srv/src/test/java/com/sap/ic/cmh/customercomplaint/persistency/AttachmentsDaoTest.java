package com.sap.ic.cmh.customercomplaint.persistency;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.customercomplaintservice.Attachments;

public class AttachmentsDaoTest {

	@InjectMocks
	@Autowired
	AttachmentsDao attachmentsDao;
	@Mock
	PersistenceService mockDb;
	@Mock
    CqnInsert cqnInsert;
	@Mock
	CqnUpdate cqnUpdate;
	@Mock
	CqnDelete cqnDelete;
	@Mock
	Result result;
	@Mock
	DraftService draftService;

	Attachments attachments;

	@Before
	public void beforeClass() {
	  MockitoAnnotations.openMocks(this);
	  attachments = Struct.create(Attachments.class);
	  attachments.setId("1");
	  attachments.setName("Test.png");
	  attachments.setType("image/png");
	}

	@Test
	public void testInsertAttachments() {
		when(result.single(Attachments.class)).thenReturn(attachments);
		when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
		attachmentsDao.insertAttachments(attachments);
	}
	@Test
	public void testRenameAttachments() {
		Map<String,Object> data = new HashMap<>();
		data.put("name", "musicTest");
		when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
		attachmentsDao.renameAttachments(data, attachments.getId());
	}
	@Test
	public void testDeleteAttachments() {
		when(mockDb.run(any(CqnDelete.class))).thenReturn(result);
		attachmentsDao.deleteAttachments(attachments.getId());
	}
	@Test
	public void testGetAttachmentDetailsBasedOnId() {
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		attachmentsDao.getAttachmentDetailsBasedOnId(attachments.getId());
	}
	
	@Test
	public void testGetDraftAttachmentDetailsBasedOnComplaintId() {
		attachments.setHasActiveEntity(false);
		attachments.setIsActiveEntity(false);
		when(draftService.run(any(CqnSelect.class))).thenReturn(result);
		attachmentsDao.getDraftAttachmentDetailsBasedOnComplaintId(attachments.getParentID());
	}
	
	@Test
	public void testGetAttachmentDetailsBasedOnComplaintId() {
		attachments.setIsActiveEntity(true);
		when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
		attachmentsDao.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID());
	}
	
	@Test
	public void testGetDraftAttachmentDetailsHasActiveEntity() {
		attachments.setHasActiveEntity(true);
		attachments.setIsActiveEntity(false);
		when(draftService.run(any(CqnSelect.class))).thenReturn(result);
		attachmentsDao.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID());
	}
	
	

}
