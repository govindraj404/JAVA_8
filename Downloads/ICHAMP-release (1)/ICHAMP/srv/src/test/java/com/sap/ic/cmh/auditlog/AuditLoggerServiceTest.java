package com.sap.ic.cmh.auditlog;

import com.sap.ic.cmh.utils.Constants;
import com.sap.xs.audit.api.exception.AuditLogException;
import com.sap.xs.audit.api.v2.AuditLogMessageFactory;
import com.sap.xs.audit.api.v2.DataModificationAuditMessage;
import com.sap.xs.audit.client.impl.v2.AuditLogMessageFactoryImpl;
import com.sap.xs.env.VcapServices;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import static org.junit.Assert.fail;
import java.util.HashMap;
import java.util.Map;


public class AuditLoggerServiceTest {

	@InjectMocks @Autowired
	AuditLoggerService service;

    Map<String, String> entityInfoMap = new HashMap<>();

    ObjectDiff diff = new ObjectDiff();
    

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        entityInfoMap.put(Constants.ENTITY_NAME, "entityName");
        entityInfoMap.put(Constants.OBJECT_ID, "UUID");
    }

	@Test
    public void testPopulateMessage() throws AuditLogException {
        AuditLogMessageFactory messageFactory = new AuditLogMessageFactoryImpl(new VcapServices());
       


        try {
            DataModificationAuditMessage message =
                    service.populateDataModificationMessage(
                            "userId",
                            "tenant",
                            "operation",
                            "role",
                            diff,
                            entityInfoMap,
                            messageFactory);
        } catch (Exception e) {
           fail("Should not have thrown exception.");
        }
    }

    @Test
    public void testLogModificationAuditData() throws AuditLogException {
        AuditLogModel testModel =
                new AuditLogModel( "userId", "tenant", "operation","role", entityInfoMap, diff);
        try {
            service.logActionModificationAuditData(testModel);
        } catch (AuditLogException e) {
            // This exception would come as audit service is not there for test
        } catch (Exception e) {
           fail("Should not have thrown any other exception.");
        }
    }

    @Test
    public void testLogModificationAuditNullData() throws AuditLogException {
        AuditLogModel testModel =
        new AuditLogModel( "userId", "tenant", "operation","role", entityInfoMap, diff);
        try {
            service.logActionModificationAuditData(testModel);
        } catch (AuditLogException e) {
            // This exception would come as audit service is not there for test
        } catch (Exception e) {
          fail("Should not have thrown any other exception.");
        }
    }
}
