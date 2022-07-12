package com.sap.ic.cmh.customercomplaint.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.ic.cmh.customercomplaint.persistency.AttachmentsDao;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.PlatformUtil;

import cds.gen.customercomplaintservice.Attachments;

@Service
public class AttachmentOperationsServiceImpl implements AttachmentOperationsService {

	@Autowired
	PlatformUtil platformUtil;
	@Autowired
	AttachmentsDao attachmentsDao;

	public static final Logger logger = LoggerHelper.getLogger(AttachmentOperationsServiceImpl.class);
	private static final String ATTACHMENT_OPERATIONS_SERVICE="AttachmentOperationsServiceImpl";

	private static final String RENAME = "Attachment Renamed";
	private static final String DELETED = "Record Deleted";
    
	/**
	 * Method to insert the attachment details to DB
	 */
	@Override
	public void storeAttachmentInHanaDb(String attachmentName, String documentId, String size,
			String attachmentContentType,String id) throws IllegalArgumentException {
		LoggerHelper.logMethodEntry(logger, ATTACHMENT_OPERATIONS_SERVICE, "storeAttachmentInHanaDb");
	
			Attachments attachmentDocuments = Struct.create(Attachments.class);
			 attachmentDocuments.setName(attachmentName);
			 attachmentDocuments.setId(id);
			 attachmentDocuments.setType(attachmentContentType);
             attachmentDocuments.setSize(size);
            attachmentDocuments.setParentID(documentId);
			 attachmentsDao.insertAttachments(attachmentDocuments);
		LoggerHelper.logMethodExit(logger, ATTACHMENT_OPERATIONS_SERVICE, "storeAttachmentInHanaDb");
	}
    
	/**
	 * Method to rename the attachment name
	 */
	@Override
	public String renameAttachment(String newName, String id) throws IllegalArgumentException {
		LoggerHelper.logMethodEntry(logger, ATTACHMENT_OPERATIONS_SERVICE, "renameAttachment");
			Map<String, Object> data = Collections.singletonMap(Attachments.NAME, newName);
			attachmentsDao.renameAttachments(data, id);
		LoggerHelper.logMethodExit(logger, ATTACHMENT_OPERATIONS_SERVICE, "renameAttachment");
		return RENAME;
	}
    
	/**
	 * Method to check Malware in the uploaded file
	 */
	@Override
	public String checkForMalware(byte[] bytes) {
		LoggerHelper.logMethodEntry(logger, ATTACHMENT_OPERATIONS_SERVICE, "checkForMalware");
		String malware = null;
		JSONObject credentials = platformUtil.getCredentials("malware-scanner");

		if (credentials != null) {
			String url = credentials.getString("uri");
			String password = credentials.getString("password");
			String user = credentials.getString("username");
			HttpHeaders headers = new HttpHeaders();
			headers.setBasicAuth(user, password);
			HttpEntity<byte[]> entity = new HttpEntity<>(bytes, headers);
			ResponseEntity<String> response = new RestTemplate().exchange("https://" + url + "/scan", HttpMethod.POST,
					entity, String.class);
			JSONObject creds = new JSONObject(response.getBody().toString());
			malware = creds.get("malwareDetected").toString();
		}
		LoggerHelper.logMethodExit(logger, ATTACHMENT_OPERATIONS_SERVICE, "checkForMalware");
		return malware;
	}
    
	/**
	 * Method to delete the attachment details from the DB
	 */
	@Override
	public String deleteAttachmentInHanaDb(String fileName) throws IllegalArgumentException {
		LoggerHelper.logMethodEntry(logger, ATTACHMENT_OPERATIONS_SERVICE, "deleteAttachmentInHanaDb");
			attachmentsDao.deleteAttachments(fileName);				
		LoggerHelper.logMethodExit(logger, ATTACHMENT_OPERATIONS_SERVICE, "deleteAttachmentInHanaDb");
		return DELETED;
	}
	
	/**
	 * Method to fetch the Attachment details based on the ID
	 */
	public Attachments getAttachmentDetails(String id) {
		Result attachmentDetailsBasedOnIdResult = attachmentsDao.getAttachmentDetailsBasedOnId(id);
		return attachmentDetailsBasedOnIdResult.first().isPresent() ? 
				attachmentDetailsBasedOnIdResult.listOf(Attachments.class).get(0) : null;
	}
	/**
	 * Get the attachment details from Draft DB based on Complaint ID
	 */
	public List<Attachments> getDraftAttachmentDetailsBasedOnComplaintId(String complaintId) {
		Result draftAttachmentDetailsBasedOnIdResult = attachmentsDao.getDraftAttachmentDetailsBasedOnComplaintId(complaintId);
		return draftAttachmentDetailsBasedOnIdResult.first().isPresent() ? 
				draftAttachmentDetailsBasedOnIdResult.listOf(Attachments.class) : new ArrayList<>();
	}
	
	/**
	 * Get the attachment details from DB based on complaint id
	 */
	public List<Attachments> getAttachmentDetailsBasedOnComplaintId(String complaintId) {
		Result draftAttachmentDetailsBasedOnIdResult = attachmentsDao.getAttachmentDetailsBasedOnComplaintId(complaintId);
		return draftAttachmentDetailsBasedOnIdResult.first().isPresent() ? 
				draftAttachmentDetailsBasedOnIdResult.listOf(Attachments.class) : new ArrayList<>();
	}
	/**
	 * Get the attachment details from Draft DB with HasActiveEntity as true
     * For Edit - Delete and save
	 */
    public List<Attachments> getDraftAttachmentDetailsHasActiveEntity(String complaintId) {
		Result draftAttachmentDetailsBasedOnIdResult = attachmentsDao.getDraftAttachmentDetailsHasActiveEntity(complaintId);
		return draftAttachmentDetailsBasedOnIdResult.first().isPresent() ? 
				draftAttachmentDetailsBasedOnIdResult.listOf(Attachments.class) : new ArrayList<>();
	}
    

}