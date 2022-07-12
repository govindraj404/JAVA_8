package com.sap.ic.cmh.auditlog;

import org.junit.Before;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.HashMap;
import java.util.Map;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;



public class AuditLogModelTest {

	@InjectMocks @Autowired
	AuditLogModel auditLogModel;

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
	}


	
	@Test
	public void testSetUserId() {
		auditLogModel.setUserId("123");
		assertEquals("123",auditLogModel.getUserId());
	}
	
	@Test
	public void testSetTenant() {
		auditLogModel.setTenant("tenant");
		assertEquals("tenant",auditLogModel.getTenant());
	}
	
	@Test
	public void testSetOperation() {
		auditLogModel.setOperation("operation");
		assertEquals("operation",auditLogModel.getOperation());
	}
	
	@Test
	public void testSetRoles() {
		auditLogModel.setRoles("Role");
		assertEquals("Role",auditLogModel.getRoles());
	}

    @Test
    public void testSetDiff(){
        ObjectDiff diff = new ObjectDiff();
        auditLogModel.setDiff(diff);
    }

    @Test
    public void testSetEntityInfoMap(){
        Map<String, String> entityInfoMap = new HashMap<>();
        entityInfoMap.put(Constants.ENTITY_NAME, "entityName");
        entityInfoMap.put(Constants.OBJECT_ID, "UUID");
        auditLogModel.setEntityInfoMap(entityInfoMap);
    }
	

}