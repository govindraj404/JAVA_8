package com.sap.ic.cmh.customercomplaint.persistency;

import java.util.Map;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.customercomplaintservice.Attachments;
import cds.gen.customercomplaintservice.Attachments_;

@Repository
public class AttachmentsDao {
	
    private DraftService draftService;

    PersistenceService db;

    AttachmentsDao(@Qualifier("CustomerComplaintService") DraftService draftService, PersistenceService db) {
        this.draftService = draftService;
        this.db = db;
    }
	
	/**
	 * Insert the attachment details into DB
	 */
	public void insertAttachments(Attachments attachments) {
		draftService.newDraft(Insert.into(Attachments_.class).entry(attachments)).single(Attachments.class);
	}
	
	/**
	 * Rename the Filename of the attachment
	 */
	public void renameAttachments(Map<String, Object> data,String id) {
		 CqnUpdate update = Update.entity(Attachments_.class).data(data)
					 		.where(b -> b.ID().contains(id).and(b.ID().eq(id)));
					 db.run(update);
	}
	
	/**
	 * Delete the attachment details from DB
	 */
	public void deleteAttachments(String attachmentId) {
		 CqnDelete delete = Delete.from(Attachments_.class)
					 		.where(b -> b.ID().eq(attachmentId));
					 db.run(delete);
	}
	
	/**
	 * Get the attachment details from DB
	 */
	public Result getAttachmentDetailsBasedOnId(String id) {
		CqnSelect select = Select.from(Attachments_.class)
					 		.where(b -> b.ID().eq(id));
			return db.run(select);
	}

	/**
	 * Get the attachment details from Draft DB
	 */
	public Result getDraftAttachmentDetailsBasedOnComplaintId(String complaintId) {
		CqnSelect select = Select.from(Attachments_.class)
					 		.where(b -> b.parentID().eq(complaintId).and(b.IsActiveEntity().eq(false)
                             .and(b.HasActiveEntity().eq(false))));
			return draftService.run(select);
	}
	
	/**
	 * Get the attachment details from DB based on complaint id
	 */
	public Result getAttachmentDetailsBasedOnComplaintId(String complaintId) {
		CqnSelect select = Select.from(Attachments_.class)
				.where(b -> b.parentID().eq(complaintId).and(b.IsActiveEntity().eq(true)));
		return db.run(select);
	}
	
	/**
	 * Get the attachment details from Draft DB with HasActiveEntity as true
     * For Edit - Delete and save
	 */
	public Result getDraftAttachmentDetailsHasActiveEntity(String complaintId) {
		CqnSelect select = Select.from(Attachments_.class)
					 		.where(b -> b.parentID().eq(complaintId).and(b.IsActiveEntity().eq(false)
                             .and(b.HasActiveEntity().eq(true))));
			return draftService.run(select);
	}


}
