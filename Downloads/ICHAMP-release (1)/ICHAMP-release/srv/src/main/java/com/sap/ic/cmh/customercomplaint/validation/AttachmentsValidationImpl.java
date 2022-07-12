package com.sap.ic.cmh.customercomplaint.validation;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.customercomplaint.service.AttachmentOperationsService;
import cds.gen.customercomplaintservice.Attachments;


@Component
public class AttachmentsValidationImpl implements AttachmentsValidation{
    
    @Autowired
    LocaleMessageHelper localeMessageHelper;
    @Autowired
	AttachmentOperationsService attachmentOperationsService;

    public static final Logger logger = LoggerHelper.getLogger(AttachmentsValidationImpl.class);
    private static final String ATTACHMENT_VALIDATION_IMPL = "AttachmentsValidationImpl";
	private static final String VALIDATE_ATTACHMENTS = "validateAttachments";
    private static final long FILE_MAX_SIZE = 5242880;


    @Override
    public String validateAttachments(MultipartFile file,String fileSize){
    	LoggerHelper.logMethodEntry(logger, ATTACHMENT_VALIDATION_IMPL, VALIDATE_ATTACHMENTS);
        //validate size and mime type
        String performSizeCheckMessage = performSizeCheck(fileSize);
        LoggerHelper.logMethodExit(logger, ATTACHMENT_VALIDATION_IMPL, VALIDATE_ATTACHMENTS);
		return StringUtils.isBlank(performSizeCheckMessage) ? performMimeTypeCheck(file) :performSizeCheckMessage ;
        
       
    }
    
    /**
     * Validate the Size of the file
     */
    public String performSizeCheck(String fileSize) {
    	LoggerHelper.logMethodEntry(logger, ATTACHMENT_VALIDATION_IMPL, VALIDATE_ATTACHMENTS);
    	String messageKey = "";
        	  logger.info("fileSize :::: {} ",fileSize);
        	  logger.info("FILE_MAX_SIZE :::: {} ",FILE_MAX_SIZE);
            if(Long.parseLong(fileSize)>FILE_MAX_SIZE) {
            	messageKey = localeMessageHelper.getMessage(MessageKeys.FILE_SIZE_VALIDATION_ERROR);
            	logger.error("The size of the uploaded photo exceeds 5 MB.");
            }
          LoggerHelper.logMethodExit(logger, ATTACHMENT_VALIDATION_IMPL, VALIDATE_ATTACHMENTS);
            return messageKey;
    }
    
    /**
     * Validate the MIME Type of the File
     */
    public String performMimeTypeCheck(MultipartFile file) {
    	LoggerHelper.logMethodEntry(logger, ATTACHMENT_VALIDATION_IMPL, VALIDATE_ATTACHMENTS);
        String messageKey = "";
            //check the mimetype and throw error 
        	logger.info("file.getContentType() :: {} ",file.getContentType());
            List<String> allowedMimetypeList = allowedMimeTypes();
            if(!allowedMimetypeList.contains(file.getContentType())){
            	messageKey = localeMessageHelper.getMessage(MessageKeys.FILE_MIME_TYPE_VALIDATION_ERROR);
                logger.error("Only files of type jpeg, png, gif, docx, doc, xls, xlsx, and pdf are allowed for uploading.");
            }
        LoggerHelper.logMethodExit(logger, ATTACHMENT_VALIDATION_IMPL, VALIDATE_ATTACHMENTS);
		return messageKey;
    }
    
    /**
     * Pre-defined Mime types that are allowed
     * to be uploaded
     */
    public List<String> allowedMimeTypes(){
        List<String> allowedMimetypeList = new ArrayList<>();
        allowedMimetypeList.add(Constants.MIME_TYPE_JPEG);
        allowedMimetypeList.add(Constants.MIME_TYPE_PNG);
        allowedMimetypeList.add(Constants.MIME_TYPE_GIF);
        allowedMimetypeList.add(Constants.MIME_TYPE_DOCX);
        allowedMimetypeList.add(Constants.MIME_TYPE_DOC);
        allowedMimetypeList.add(Constants.MIME_TYPE_XLS);
        allowedMimetypeList.add(Constants.MIME_TYPE_XLSX);
        allowedMimetypeList.add(Constants.MIME_TYPE_PDF);
        return allowedMimetypeList;
    }

    /**
     * Check if the Attachment exists in DB
     */
	@Override
	public String validateAttachmentExists(String id) {
		Attachments attachmentDetails = attachmentOperationsService.getAttachmentDetails(id);
		return null!=attachmentDetails ? attachmentDetails.getName() : "";
	}
    
}
