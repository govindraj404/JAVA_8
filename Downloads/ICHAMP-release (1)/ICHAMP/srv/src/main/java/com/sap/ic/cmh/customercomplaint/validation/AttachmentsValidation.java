package com.sap.ic.cmh.customercomplaint.validation;

import org.springframework.web.multipart.MultipartFile;

public interface AttachmentsValidation {

    public String validateAttachments(MultipartFile file,String fileSize);
    public String validateAttachmentExists(String id);
    
}
