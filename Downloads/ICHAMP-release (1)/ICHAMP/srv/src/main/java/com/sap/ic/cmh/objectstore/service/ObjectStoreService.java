package com.sap.ic.cmh.objectstore.service;


import com.sap.ic.cmh.objectstore.model.UploadStatus;

public interface ObjectStoreService {

    UploadStatus uploadFile(String subdomain, byte[] bytes, String name, String contentType);

}
