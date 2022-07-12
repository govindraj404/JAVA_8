package com.sap.ic.cmh.objectstore.service;

import com.sap.ic.cmh.objectstore.config.AzureStorageConfiguration;
import com.sap.ic.cmh.objectstore.model.UploadStatus;
import com.sap.ic.cmh.objectstore.repository.ObjectStoreRepository;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class AzureObjectStoreService implements ObjectStoreService {

    private final AzureStorageConfiguration azureConfig;
    private final ObjectStoreRepository repository;
    private static Logger logger = LoggerFactory.getLogger(AzureObjectStoreService.class);

    @Autowired
    ObjectStoreUtil objectStoreUtil;

    @Autowired
    public AzureObjectStoreService(final AzureStorageConfiguration azureConfig, ObjectStoreRepository repository) {
        this.azureConfig = azureConfig;
        this.repository = repository;
    }

    @Override
    public UploadStatus uploadFile(String subdomain, byte[] bytes, String fileName, String contentType) {
        repository.setContext(azureConfig.getBlobStoreContext(subdomain));
        String bucket = objectStoreUtil.getBucket(subdomain);
        logger.info("Upload started");
        UploadStatus uploadStatus = repository.uploadFile(bucket, bytes, fileName, contentType);
        logger.info("upload completed");
        return uploadStatus;
    }
}
