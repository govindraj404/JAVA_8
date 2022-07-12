package com.sap.ic.cmh.attachment.service;

import java.io.InputStream;
import java.util.List;

import org.springframework.stereotype.Service;

import com.sap.ic.cmh.objectstore.model.BlobContent;
import com.sap.ic.cmh.objectstore.model.UploadStatus;

@Service
public interface AttachmentObjectStoreService {
	
	public UploadStatus uploadFile(String subdomain,byte[] bytes, String fileName, String contentType);
	public boolean deleteFile(String subdomain,String fileName);
	public InputStream getFile(String subdomain,String fileName);
	public List<BlobContent> listObjects(String subdomain);
	public List<BlobContent> listFileObjects(String subdomain, String fileName);
	public boolean isBlobExist(String subdomain,String name);
	
	

}