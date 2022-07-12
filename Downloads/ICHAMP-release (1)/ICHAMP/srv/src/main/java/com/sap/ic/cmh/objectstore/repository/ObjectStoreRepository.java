package com.sap.ic.cmh.objectstore.repository;

import com.sap.ic.cmh.objectstore.model.UploadStatus;
import com.sap.ic.cmh.objectstore.util.ObjectStoreUtil;

import org.jclouds.blobstore.BlobStore;
import org.jclouds.blobstore.BlobStoreContext;
import org.jclouds.blobstore.ContainerNotFoundException;
import org.jclouds.blobstore.domain.Blob;
import org.jclouds.blobstore.domain.PageSet;
import org.jclouds.blobstore.domain.StorageMetadata;
import org.jclouds.io.Payload;
import org.jclouds.io.payloads.ByteArrayPayload;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.ic.cmh.objectstore.model.BlobContent;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

@Repository
public class ObjectStoreRepository {

    @Autowired
    ObjectStoreUtil objectStoreUtil;

    private BlobStoreContext context;

    private static Logger logger = LoggerFactory.getLogger(ObjectStoreRepository.class);

    public BlobStoreContext getContext() {
        return context;
    }

    public void setContext(BlobStoreContext context) {
        this.context = context;
    }

    /**
     * @param bucketName
     * @param bytes
     * @param fileName
     * @return message
     */
    public UploadStatus uploadFile(String bucketName, byte[] bytes, String fileName, String contentType) {
    	BlobStore blobStore;
        UploadStatus uploadStatus = new UploadStatus();

        try {
            // getting blob store
           blobStore = getContext().getBlobStore();

           // creating payload
           Payload payload = new ByteArrayPayload(bytes);

           // adding user metadata to the blob
           Map<String, String> userMetadata = new HashMap<>();
           userMetadata.put("description", "sample content");

           // creating Blob
           Blob blob = blobStore.blobBuilder(fileName).payload(payload).contentType(contentType)
                   .userMetadata(userMetadata).build();

           // Multipart upload is currently not supported since it has an issue
           // with OpenStack Swift.
           // multipart issue:
           // (https://issues.apache.org/jira/browse/JCLOUDS-1064).
           blobStore.putBlob(bucketName, blob);
        } catch (ContainerNotFoundException e) {
            logger.info("*******************UPLOAD FAILED***********************");
            uploadStatus.setStatus("FAILURE");
            uploadStatus.setErrorMessage(e.getMessage());
        } 
       finally {
           getContext().close();
       }
        uploadStatus.setStatus("SUCCESS");        
        return uploadStatus;
    }

    /**
     * @param bucketName
     * @param fileName
     * @return InputStream
     */
    public InputStream downloadFile(String bucketName, String fileName) {
    	BlobStore blobStore;
        InputStream inputStream = null;
        try {
            // getting blobstore
            blobStore = getContext().getBlobStore();

            // getting blob
            Blob blob = blobStore.getBlob(bucketName, fileName);

            inputStream = blob.getPayload().openStream();
            logger.info("*******************DOWNLOADED SUCCESSFULLY***********************");
        } catch (IOException e) {
            logger.info("*******************DOWNLOAD FAILED***********************");
            logger.error("Error occurred while downloading the file");
        } finally {
            getContext().close();
        }

        return inputStream;
    }

    public boolean isBlobExist(String bucketName, String fileName) {
    	BlobStore blobStore;
        boolean isExist = false;
        try {
            // getting blobstore
            blobStore = getContext().getBlobStore();
            isExist = blobStore.blobExists(bucketName, fileName);
        } finally {
            getContext().close();
        }
        return isExist;
    }
    
	public boolean deleteFile(String bucketName, String fileName) {
		BlobStore blobStore;
		boolean isBlobRemoved = false;
		try {
			blobStore = getContext().getBlobStore();
			blobStore.removeBlob(bucketName,fileName);			
			if(!isBlobExist(bucketName, fileName)) {
				isBlobRemoved = true;				
			}
		} finally {
			getContext().close();
		}		
		return isBlobRemoved;
	}

    public List<BlobContent> listFiles(String bucketName) {		
		List<BlobContent> files = new ArrayList<>();
		PageSet<? extends StorageMetadata> list;	
        BlobStore blobStore;	
		try {
			blobStore = getContext().getBlobStore();			
			list = blobStore.list(bucketName);		
			if(list != null) {
				for (Iterator<? extends StorageMetadata> it = list.iterator(); it.hasNext(); ) {
					StorageMetadata storageMetadata = it.next();
					Blob blob = blobStore.getBlob(bucketName, storageMetadata.getName());
					files.add(objectStoreUtil.createBlobFile(blob));
				}
			}
		} finally {
			getContext().close();
		}		
		return files;
	}

    public List<BlobContent> listNameFiles(String bucketName,  String fileName) {
		List<BlobContent> files = new ArrayList<>();
		PageSet<? extends StorageMetadata> list;
        BlobStore blobStore;
		try {
			blobStore = getContext().getBlobStore();
			list = blobStore.list(bucketName);
			if(list!=null) {
				for(Iterator<? extends StorageMetadata> it = list.iterator(); it.hasNext();) {
					StorageMetadata storageMetadata = it.next();
					Blob blob = blobStore.getBlob(bucketName, storageMetadata.getName());
					if(storageMetadata.getName().startsWith(fileName)) {
						files.add(objectStoreUtil.createBlobFile(blob));
					}
				}
			}
		} finally {
			getContext().close();
		}		
		return files;
	}
}