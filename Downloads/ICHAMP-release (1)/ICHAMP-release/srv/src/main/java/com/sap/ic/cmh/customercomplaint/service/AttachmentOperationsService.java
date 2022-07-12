package com.sap.ic.cmh.customercomplaint.service;

import org.springframework.stereotype.Service;
import java.util.List;

import cds.gen.customercomplaintservice.Attachments;

@Service
public interface AttachmentOperationsService {
	
    public void storeAttachmentInHanaDb(String attachmentName, String documentId, String size, String attachmentContentType,String id) throws IllegalArgumentException;
    public String renameAttachment(String newName, String id) throws IllegalArgumentException;
    public String checkForMalware(byte[] bytes);
    public String deleteAttachmentInHanaDb(String id);
    public Attachments getAttachmentDetails(String id);
    public List<Attachments> getDraftAttachmentDetailsBasedOnComplaintId(String complaintId);
    public List<Attachments> getAttachmentDetailsBasedOnComplaintId(String complaintId);
    public List<Attachments> getDraftAttachmentDetailsHasActiveEntity(String complaintId);
}