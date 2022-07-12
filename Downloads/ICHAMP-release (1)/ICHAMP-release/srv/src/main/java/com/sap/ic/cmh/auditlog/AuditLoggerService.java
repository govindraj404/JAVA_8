package com.sap.ic.cmh.auditlog;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.sap.xs.audit.api.exception.AuditLogException;
import com.sap.xs.audit.api.v2.AuditLogMessageFactory;
import com.sap.xs.audit.api.v2.AuditedDataSubject;
import com.sap.xs.audit.api.v2.AuditedObject;
import com.sap.xs.audit.api.v2.DataModificationAuditMessage;
import com.sap.xs.audit.client.impl.v2.AuditLogMessageFactoryImpl;
import java.util.Map;
import com.sap.ic.cmh.utils.Constants;

@Service
public class AuditLoggerService {
	private final Logger logger = LoggerFactory.getLogger(AuditLoggerService.class);

    public void logActionModificationAuditData(AuditLogModel model) throws AuditLogException {
        logger.info("--------------LOGGING STARTED--------------");
        AuditLogMessageFactory messageFactory = new AuditLogMessageFactoryImpl();
        DataModificationAuditMessage message =
                populateDataModificationMessage(
                        model.getUserId(),
                        model.getTenant(),
                        model.getOperation(),
                        model.getRoles(),
                        model.getDiff(),
                        model.getEntityInfoMap(),
                        messageFactory);


        message.logSuccess();
        logger.info("--------------LOGGING SUCCESSFUL--------------");
    }

    public DataModificationAuditMessage populateDataModificationMessage(
            String userId,
            String tenant,
            String operation,
            String roles,
            ObjectDiff diff,
            Map<String, String> entityInfoMap,
            AuditLogMessageFactory messageFactory) {
       DataModificationAuditMessage message = messageFactory.createDataModificationAuditMessage();
        message.setTenant(tenant);
        message.setUser(userId);
        message.setDataSubject(getDataSubject(messageFactory, userId, roles));
        message.setObject(getDataObject(messageFactory, operation, entityInfoMap));
        message.setIdentityProvider(Constants.AUDIT_ID_PROVIDER);
        if(diff != null){
            message.addAttribute(diff.getAttributeName(), diff.getOldValue(), diff.getNewValue());
        }
       
        return message;
    }



    public AuditedDataSubject getDataSubject(
            AuditLogMessageFactory messageFactory, String userId, String roles) {
        AuditedDataSubject auditedDataSubject = messageFactory.createAuditedDataSubject();
        auditedDataSubject.addIdentifier(Constants.AUDIT_USER_NAME, userId);
        auditedDataSubject.setType(Constants.AUDIT_USER_TYPE);
        auditedDataSubject.setRole(roles);
        return auditedDataSubject;
    }

    public AuditedObject getDataObject(
            AuditLogMessageFactory messageFactory, String operation,  Map<String, String> entityInfoMap) {
        AuditedObject auditedObject = messageFactory.createAuditedObject();
        auditedObject.setType(entityInfoMap.get(Constants.ENTITY_NAME));
        auditedObject.addIdentifier(Constants.OBJECT_ID, entityInfoMap.get(Constants.OBJECT_ID));
        auditedObject.addIdentifier(Constants.AUDIT_OP, operation);
        return auditedObject;
    }
}
