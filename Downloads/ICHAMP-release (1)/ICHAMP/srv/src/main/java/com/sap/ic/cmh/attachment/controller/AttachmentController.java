package com.sap.ic.cmh.attachment.controller;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.sap.ic.cmh.attachment.service.AttachmentObjectStoreService;
import com.sap.ic.cmh.customercomplaint.service.AttachmentOperationsService;
import com.sap.ic.cmh.customercomplaint.validation.AttachmentsValidation;
import com.sap.ic.cmh.objectstore.model.BlobContent;
import com.sap.ic.cmh.objectstore.model.UploadStatus;
import com.sap.ic.cmh.utils.GenericUtils;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.XsuaaToken;


@RestController
@RequestMapping("/objectstorage/api")
public class AttachmentController {

	public static final Logger logger = LoggerHelper.getLogger(AttachmentController.class);

	private static final String ATTACHMENT_CONTROLLER = "AttachmentController";
	private static final String UPLOAD = "upload";
	private static final String SUBDOMAIN="Subdomain :: {} ";
    private static final String SUCCESS="SUCCESS";
	public static final String FILE_DOESNOT_EXIST = " does not exist in the container";
	public static final String DELETE_SUCCESSFUL = " is successfully deleted.";
	public static final String UPLOAD_FAILED = "Error occured while uploading the file: ";
	public static final String DELETE_FAILED = "Error occured while deleting the file: ";
    public static final String CANNOT_DELETE_NULL = "Could not delete a null object.";

    AttachmentObjectStoreService attachmentObjectStoreService;
	AttachmentOperationsService attachmentOperationsService;
    AttachmentsValidation attachmentsValidation;
    XsuaaToken xsuaaToken;


	public AttachmentController(AttachmentObjectStoreService attachmentObjectStoreService,
			AttachmentOperationsService attachmentOperationsService,AttachmentsValidation attachmentsValidation,
			XsuaaToken xsuaaToken) {
		this.attachmentObjectStoreService = attachmentObjectStoreService;
		this.attachmentOperationsService=attachmentOperationsService;
		this.attachmentsValidation=attachmentsValidation;
		this.xsuaaToken=xsuaaToken;
	}
    /**
     * Method to upload a file to objectstore and store the same in DB
     * @param file
     * @param documentId
     * @param applicationName
     * @param attachmentName
     * @param overWrite
     * @return
     * @throws FileUploadException
     */
	@PostMapping("/attachment/upload")
	public ResponseEntity<Object> upload(@RequestParam("file") MultipartFile file,
			@RequestParam("complaintId") String documentId,
			@RequestParam("attachmentName") String attachmentName) {
		LoggerHelper.logMethodEntry(logger, ATTACHMENT_CONTROLLER, UPLOAD);
		logger.info("Inside Upload method");
		String subdomain = xsuaaToken.getSubdomain();
		logger.info(SUBDOMAIN, subdomain);
			logger.info("Before Uploading ::::  ");
				String message = "";
				if (file.isEmpty()) {
					return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
				} else {
					message = uploadAttachment(file, documentId, attachmentName, subdomain, message);
				}
				LoggerHelper.logMethodExit(logger, ATTACHMENT_CONTROLLER, UPLOAD);
				return new ResponseEntity<>(message, !message.contains(SUCCESS) ?
                HttpStatus.BAD_REQUEST : HttpStatus.OK);
	}
	public String uploadAttachment(MultipartFile file, String documentId,String attachmentName,
			String subdomain, String message) {
		LoggerHelper.logMethodEntry(logger, ATTACHMENT_CONTROLLER, UPLOAD);
		try {
			byte[] bytes = file.getBytes();
			String scan = attachmentOperationsService.checkForMalware(bytes);
			String size = Integer.toString(bytes.length);
			if (scan.equals("false")) {
				logger.info("No malware detected and proceeding for Validations");
				String validationMessage = attachmentsValidation.validateAttachments(file,size);
				logger.info("validationMessage ::: {} ",validationMessage);
				  if(StringUtils.isBlank(validationMessage)) {
                      logger.info("Validation passed and no error messages");
                      String id = UUID.randomUUID().toString();
                      logger.info("Attachment ID ::: {} ",id);
                      String fileSize=String.valueOf(file.getSize());
					  UploadStatus uploadFile = attachmentObjectStoreService.uploadFile(subdomain, bytes,
                            id, file.getContentType());
						message = null!=uploadFile ? uploadFile.getStatus().concat("with name : ").concat(id) : "";
						attachmentOperationsService.storeAttachmentInHanaDb(attachmentName, documentId,
                        fileSize, file.getContentType(),id);
				  }else{
                    message = validationMessage;
                  }
			}
		} catch (IllegalArgumentException | IOException e) {
			LoggerHelper.logExceptionWithMessage(logger, UPLOAD_FAILED, e);
		} 
		LoggerHelper.logMethodExit(logger, ATTACHMENT_CONTROLLER, UPLOAD);
		return message;
	}
    
	/**
	 * Method to list all the files in the Object store
	 * @return
	 */
    @GetMapping("/attachment")
	@ResponseBody
	public ResponseEntity<List<BlobContent>> listFiles() {	
    	LoggerHelper.logMethodEntry(logger, ATTACHMENT_CONTROLLER, "listFiles");
		List<BlobContent> blobFiles = null;
		String subdomain = xsuaaToken.getSubdomain();
		logger.info(SUBDOMAIN, subdomain);
		try {
			blobFiles = attachmentObjectStoreService.listObjects(subdomain);
		} catch (IllegalArgumentException e) {
			LoggerHelper.logExceptionWithMessage(logger, "Exception", e);
		}
		LoggerHelper.logMethodExit(logger, ATTACHMENT_CONTROLLER, "listFiles");
		return new ResponseEntity<>(blobFiles, HttpStatus.OK);
	}
    
    /**
     * Method to rename the attachment
     * @param newName
     * @param documentId
     * @param attachmentId
     * @return
     */
	@PostMapping("/attachment/rename")
	public ResponseEntity<Object> renameAttachment(@RequestParam("newName") String newName,
			@RequestParam("attachmentId") String attachmentId) {
		LoggerHelper.logMethodEntry(logger, ATTACHMENT_CONTROLLER, "renameAttachment");
		   String message = attachmentOperationsService.renameAttachment(newName,attachmentId);
		LoggerHelper.logMethodExit(logger, ATTACHMENT_CONTROLLER, "renameAttachment");
			return new ResponseEntity<>(message, HttpStatus.OK);
	}
    
	/**
	 * Method to get a particular file detail based ont the file name
	 * @param fileName
	 * @return
	 */
    @GetMapping("/attachment/files/{fileName}")
	@ResponseBody
	public ResponseEntity<List<BlobContent>> getFiles(@PathVariable("fileName") String fileName) {
    	LoggerHelper.logMethodEntry(logger, ATTACHMENT_CONTROLLER, "getFiles");
		List<BlobContent> blobFiles = null;
		String subdomain = xsuaaToken.getSubdomain();
		logger.info(SUBDOMAIN, subdomain);
		try {
			blobFiles = attachmentObjectStoreService.listFileObjects(subdomain,fileName);
		} catch (IllegalArgumentException e) {
			LoggerHelper.logExceptionWithMessage(logger, "Exception", e);
		}
		LoggerHelper.logMethodExit(logger, ATTACHMENT_CONTROLLER, "getFiles");
		return new ResponseEntity<>(blobFiles, HttpStatus.OK);
	}
    
    /**
     * Method to download a file
     */
    @GetMapping(value = "/attachment/{fileName:.*}")
	public ResponseEntity<InputStreamResource> getDetailsAndDownloadFile(@PathVariable(value = "fileName") String fileName) {
    	LoggerHelper.logMethodEntry(logger, ATTACHMENT_CONTROLLER, "getDetailsAndDownloadFile");
    	String subdomain = xsuaaToken.getSubdomain();
		logger.info(SUBDOMAIN, subdomain);
		InputStreamResource inputStreamResource = null;
			String attachmentName = attachmentsValidation.validateAttachmentExists(fileName);
			if(StringUtils.isNotBlank(attachmentName)) {
				HttpHeaders respHeaders = new HttpHeaders();
				if (attachmentObjectStoreService.isBlobExist(subdomain, fileName)) {
					respHeaders.setContentDispositionFormData("attachment", fileName);
					inputStreamResource = new InputStreamResource(attachmentObjectStoreService.getFile(subdomain,fileName));
				}
				
				return ResponseEntity.ok().header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + attachmentName)
		                .contentType(MediaType.parseMediaType("application/octet-stream"))
						.body(inputStreamResource);
			}else {
				return ResponseEntity.badRequest().headers(HttpHeaders.EMPTY)
						.contentType(MediaType.parseMediaType("text/plain"))
						.body(inputStreamResource);
			}
	}
    
	/**
	 * Method to delete an attachment from Objectstore and from DB
	 * @param fileName
	 * @return
	 */
    @DeleteMapping("/attachment/{fileName}")
	public ResponseEntity<String> deleteAttachment(@PathVariable(value = "fileName") String fileName) {
    	LoggerHelper.logMethodEntry(logger, ATTACHMENT_CONTROLLER, "deleteAttachment");
		String msg = CANNOT_DELETE_NULL;
		String subdomain = xsuaaToken.getSubdomain();
        logger.info(SUBDOMAIN, subdomain);
		if (fileName != null) {
			if (attachmentObjectStoreService.isBlobExist(subdomain, fileName)) {
				if (attachmentObjectStoreService.deleteFile(subdomain,fileName)) {
					msg = fileName + DELETE_SUCCESSFUL;
					logger.info("Success msg :::: {} ",msg);
										
				} else {
					msg = DELETE_FAILED + fileName;
					logger.info("Failed msg :::: {} ",msg);
					return new ResponseEntity<>(msg, HttpStatus.INTERNAL_SERVER_ERROR);
				}
			} else {
				msg = fileName + FILE_DOESNOT_EXIST;
				logger.info("Error msg :::: {} ",msg);
				return errorMessage(msg, HttpStatus.NOT_FOUND);
			}
		}
		LoggerHelper.logMethodExit(logger, ATTACHMENT_CONTROLLER, "deleteAttachment");
		return new ResponseEntity<>(msg, HttpStatus.OK);
	}	
    
    /**
     * Method to form the error message 
     * @param message
     * @param status
     * @return
     */
	public static ResponseEntity errorMessage(String message, HttpStatus status) {
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(org.springframework.http.MediaType.TEXT_PLAIN);

		return ResponseEntity.status(status).headers(headers).body(GenericUtils.sanitize(message));
	}

}