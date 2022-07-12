package com.sap.ic.cmh.attachment.service;

import java.io.InputStream;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import com.sap.ic.cmh.objectstore.config.AzureStorageConfiguration;
import com.sap.ic.cmh.objectstore.model.BlobContent;
import com.sap.ic.cmh.objectstore.model.UploadStatus;
import com.sap.ic.cmh.objectstore.repository.ObjectStoreRepository;
import com.sap.ic.cmh.objectstore.service.AzureObjectStoreService;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;

@Profile("cloud")
@Service
public class AttachmentObjectStoreServiceImpl implements AttachmentObjectStoreService {
	
	private final AzureStorageConfiguration azureConfig;
	private final ObjectStoreRepository repository;
    private AzureObjectStoreService azureObjectStoreService;
	ObjectStoreUtil objectStoreUtil;

	@Autowired
	public AttachmentObjectStoreServiceImpl(AzureStorageConfiguration azureConfig, 
    ObjectStoreRepository repository,AzureObjectStoreService azureObjectStoreService,ObjectStoreUtil objectStoreUtil) {
		this.azureConfig = azureConfig;
		this.repository = repository;
        this.azureObjectStoreService=azureObjectStoreService;
        this.objectStoreUtil=objectStoreUtil;
	}
    
	/**
	 * Upload a file to the Object Store
	 */
	@Override
	public UploadStatus uploadFile(String subdomain,byte[] bytes, String fileName, String contentType) {
		repository.setContext(azureConfig.getBlobStoreContext(subdomain));
		return azureObjectStoreService.uploadFile(subdomain, bytes, fileName, contentType);
	}
    
	/**
	 * Delete a file from Object Store
	 */
	@Override
	public boolean deleteFile(String subdomain,String fileName) {
		repository.setContext(azureConfig.getBlobStoreContext(subdomain));
        String bucket = objectStoreUtil.getBucket(subdomain);
		return repository.deleteFile(bucket, fileName);
	}
    
	/**
	 * Get/download a file from Object Store based on File name
	 */
	@Override
	public InputStream getFile(String subdomain,String fileName) {
        repository.setContext(azureConfig.getBlobStoreContext(subdomain));
		String bucketContainer = objectStoreUtil.getBucket(subdomain);
		return repository.downloadFile(bucketContainer, fileName);
	}
    
	/**
	 * List all the files from the Object Store
	 */
	@Override
	public List<BlobContent> listObjects(String subdomain) {
		repository.setContext(azureConfig.getBlobStoreContext(subdomain));
		String bucketContainer = objectStoreUtil.getBucket(subdomain);
		return repository.listFiles(bucketContainer);
	}
    
	/**
	 * List a particular file based on file name from Object Store
	 */
	@Override
	public List<BlobContent> listFileObjects(String subdomain,String fileName) {
		repository.setContext(azureConfig.getBlobStoreContext(subdomain));
		String bucketContainer = objectStoreUtil.getBucket(subdomain);
		return repository.listNameFiles(bucketContainer, fileName);
	}
    
	/**
	 * Check if File exists in the Object Store
	 */
	@Override
	public boolean isBlobExist(String subdomain,String fileName) {
		repository.setContext(azureConfig.getBlobStoreContext(subdomain));
        String bucket = objectStoreUtil.getBucket(subdomain);
		return repository.isBlobExist(bucket, fileName);
	}

}