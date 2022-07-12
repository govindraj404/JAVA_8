package com.sap.ic.cmh.utils;



import cds.gen.manageclaimservice.Claims;
import cds.gen.managecomplaintservice.Complaints;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.managesupplierissueprocessservice.Supplier8DProcesses;
import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;
import com.sap.ic.cmh.claim.model.binary_relation.DocumentFlow;
import com.sap.ic.cmh.network.service.HttpService;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

import java.util.*;

public class CommonFunctionsTest {
	@InjectMocks
	@Autowired
	CommonFunctions cf;

	@Mock
	ParameterInfo parameterInfo;

	@Mock
	CdsReadEventContext context;
	@Mock
	HttpService httpService;

	private Complaints complaints;
	private Claims claims;
	private Supplier8DProcesses supplier8d;
	private ReturnPurchaseOrders returnPurchaseOrders;
//	Map<String, Object> draft = new LinkedHashMap<>();
//	Map<String, Object> sibling = new LinkedHashMap<>();
//	Map<String, Object> draftOrder = new LinkedHashMap<>();
//	Map<String, Object> siblingOrder = new LinkedHashMap<>();

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		complaints = Struct.create(Complaints.class);
		claims = Struct.create(Claims.class);
		supplier8d = Struct.create(Supplier8DProcesses.class);
		returnPurchaseOrders = Struct.create(ReturnPurchaseOrders.class);
		claims.setId("201");
		complaints.setCompanyCodeId("FP01");
		complaints.setPlantId("11");
		complaints.setId("EightDID");
		complaints.setComplaintStatusCode("CRTD");
		complaints.setSupplierId("test");
		
		supplier8d.setCompanyId("companyID");
		returnPurchaseOrders.setCompanyId("compantID");


	}

	@Test
	public void testConvertObjectToMap() {
		cf.convertObjectToMap(complaints);
	}



	@Test
	public void testConvertManageComplaintsToComplaints() {
		cf.convertManageComplaintsToComplaints(complaints);
	}


	@Test
	public void testconvertManageClaimsToClaims() {
		cf.convertManageClaimsToClaims(claims);
	}

	@Test
	public void testConvertManageReturnPOtoReturnOrders() {
		cf.convertManageReturnPOtoReturnOrders(returnPurchaseOrders);
	}

	@Test
	public void testConvertManageSupplierEightDToSupplierEightD() {
		cf.convertManageSupplierEightDToSupplierEightD(supplier8d);
	}

	@Test
	public void testConvertClaimsToManageClaims() {
		cds.gen.claimservice.Claims claim = Struct.create(cds.gen.claimservice.Claims.class);
		claim.setIsActiveEntity(false);
		claim.setHasActiveEntity(false);
		claim.setHasDraftEntity(false);
		claim.setIsClaimFieldControl(10);
		claim.setIsUpdateRestricted(false);
		claim.setSNavigation("s-n");
		cf.convertClaimsToManageClaims(claim);
	}


	@Test
	public void testConvertComplaintToManageComplaint() {
		cds.gen.complaintservice.Complaints com = Struct.create(cds.gen.complaintservice.Complaints.class);
		com.setIsActiveEntity(false);
		com.setHasActiveEntity(false);
		com.setHasDraftEntity(false);
		com.setIsComplaintNew(10);
		com.setIsFieldControlMandatory(10);
		com.setIsHideAdaptStreams(false);
		com.setIsHideCostCollection(false);
		cf.convertComplaintToManageComplaint(com);
	}

	@Test
	public void testConvertManageQNToDraftQualityNotifications() {
		cds.gen.managequalitynotificationservice.QualityNotifications com = Struct.create(cds.gen.managequalitynotificationservice.QualityNotifications.class);
		cf.convertManageQualitynotificationtoQualityNotifications(com);
	}

	@Test
	public void testCnvertManageDefectsToDraftDefects() {
		cds.gen.managequalitynotificationservice.Defects defects = Struct.create(cds.gen.managequalitynotificationservice.Defects.class);
		cf.convertManageDefectsToDraftDefects(defects);
	}

	@Test
	public void testConvertInternalSupplierEightDToSupplierEightD() {
		cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses internalSupplierIssueProcesses = Struct.create(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
		cf.convertInternalSupplierEightDToSupplierEightD(internalSupplierIssueProcesses);
	}


	@Test
	public void testConvertManageCostCollectorToCostCollector() {
		cds.gen.managecomplaintservice.CostCollectors costCollectors = Struct.create(cds.gen.managecomplaintservice.CostCollectors.class);
		cf.convertManageCostCollectorToCostCollector(costCollectors);
	}

	@Test
	public void testDocumentFlow() {
		DocumentFlow setDocumentFlowRequest = cf.setDocumentFlowRequest("1234", "BUS2078", "", "BUS222", "ASD", "ASD");
		assertEquals("1234", setDocumentFlowRequest.getObjectANumber());
		assertEquals("BUS2078", setDocumentFlowRequest.getObjectAType());
		assertEquals("", setDocumentFlowRequest.getObjectBNumber());
		assertEquals("BUS222", setDocumentFlowRequest.getObjectBType());
		assertEquals("ASD", setDocumentFlowRequest.getSourceLogicalSystem());
		assertEquals("ASD", setDocumentFlowRequest.getTargetLogicalSystem());
		assertNotNull(setDocumentFlowRequest.getObjectA());
		assertNotNull(setDocumentFlowRequest.getObjectB());

	}

	@Test
	public void testSetBinaryRelationDataModel() {
		BinaryRelationDataModel setBinaryRelationDataModel = cf.setBinaryRelationDataModel("1234", "BUS2078", "BUS222", "ASD", "ASD");
		assertNotNull(setBinaryRelationDataModel.getObjectA());
		assertNotNull(setBinaryRelationDataModel.getObjectB());
		assertEquals("VONA", setBinaryRelationDataModel.getRelationType());
	}

	@Test
	public void testCnvertManageDefectsToDraftDefectsNull() {
		cf.convertManageDefectsToDraftDefects(null);
	}

	@Test
	public void testAfterBTPUsersRead() {
		Map<String, String> map1 =new HashMap<>();
		map1.put("$expand" , " ");
		map1.put("$select", " ");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		cf.checkBeforeRead(context);
	}

	@Test
	public void testAfterBTPUsersReadIfElseCondition() {
		Map<String, String> map1 =new HashMap<>();
		map1.put("$expand" , " ");
		map1.put("$select", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		cf.checkBeforeRead(context);
	}

	@Test
	public void testAfterBTPUsersReadIfElseValidate() {
		Map<String, String> map1 =new HashMap<>();
		map1.put("$expand" , null);
		map1.put("$select", " ");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		cf.checkBeforeRead(context);
	}

	@Test
	public void testCheckBeforeReadElseCondition() {
		Map<String, String> map1 =new HashMap<>();
		map1.put("$expand" , null);
		map1.put("$select", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		cf.checkBeforeRead(context);
	}

    @Test
	public void testRemoveAttributes() {
		List<String> attributeList = new ArrayList<String>();
		Map<String,Object> businessObjectMap = new HashMap<>();
		businessObjectMap.put("ID", 1);
		businessObjectMap.put("type", "NB2");
		businessObjectMap.put("ID1", 2);
		businessObjectMap.put("ID2", 3);
		attributeList.add("ID");
		attributeList.add("ID1");
		cf.removeAttributes(attributeList, businessObjectMap);
		
	}
}