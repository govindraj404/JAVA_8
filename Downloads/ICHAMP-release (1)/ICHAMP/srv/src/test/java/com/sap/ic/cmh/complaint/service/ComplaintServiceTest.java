package com.sap.ic.cmh.complaint.service;

import cds.gen.complaintservice.CommonBusinessObjects;
import cds.gen.complaintservice.ComplaintStatuses;
import cds.gen.complaintservice.ComplaintCategories;
import cds.gen.complaintservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;
import cds.gen.masterdataservice.BusinessPartners;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cloud.sdk.cloudplatform.connectivity.Destination;
import com.sap.cloud.sdk.cloudplatform.connectivity.DestinationAccessor;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.cds.services.messages.Messages;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import com.sap.ic.cmh.utils.Constants;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

public class ComplaintServiceTest {

	@InjectMocks
	@Autowired
	ComplaintServiceImpl complaintService;
	@Mock
	ComplaintsDao complaintDao;
	@Mock
	private PersistenceService db;
	@Mock
	Result result;

	@Mock
	CostCollectorService costCollectorService;
	@Mock
	ConfigurationService configurationService;
	@Mock
	BusinessPartnerRepository businessPartnerRepository;
	@Mock
	ComplaintValidation complaintValidation;

	@Mock
	DestinationAccessor accessor;

	@Mock
	Destination destination;

	@Mock
	BusinessPartnerService businessPartnerService;

	@Mock
	BusinessObjectDao businessObjectDao;

	@Mock
	Messages messages;

	private CostCollectors cost;
	private Complaints complaint;
	private List<Complaints> complaintList = new ArrayList<>();
	private List<CostCollectors> costList = new ArrayList<>();
	private Row row;
	private Optional<Row> opt;

	@Before
	public void before() {
		MockitoAnnotations.openMocks(this);
		cost = Struct.create(CostCollectors.class);
		cost.setTotalCost(BigDecimal.valueOf(10));
		cost.setQuantity(BigDecimal.valueOf(10));
		costList.add(cost);

		complaint = Struct.create(Complaints.class);
		complaint.setId("12345");
		complaint.setTotalLaborHour(null);
		complaint.setTotalSubLetCost(null);
		complaint.setCurrencyCode(null);
        complaint.setCreationType("Automatic");
        complaint.setReferenceNumber("123");
		complaintList.add(complaint);

		row = Struct.create(Row.class);
	}

	@Test
	public void testGetComplaint() {
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		complaintService.getComplaintDetails(complaint.getId());
	}

	@Test
	public void testUpdateComplaintStatus() {
		complaintService.updateComplaintStatus("ComplaintID", "CRTD");
	}

	@Test
	public void testUpdateComplaintWithCost() {
		BigDecimal tc = new BigDecimal("10");
		complaint.setTotalSubLetCost(new BigDecimal(10));
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.updateComplaintWithCost(complaint.getId(),tc);
	}

	@Test
	public void testUpdateComplaintWithCostZero() {
		BigDecimal tc = new BigDecimal("0");
		complaint.setTotalSubLetCost(new BigDecimal(10));
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "100");
		row.put("name", "F001");
		row.put("descr", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(result.listOf(Complaints.class)).thenReturn(complaintList);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.updateComplaintWithCost(complaint.getId(),tc);
	}
	@Test
	public void testUpdateComplaintWithCostLessThanZero() {
		BigDecimal tc = new BigDecimal("0");
		complaint.setTotalSubLetCost(new BigDecimal(-0.01));
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.updateComplaintWithCost(complaint.getId(),tc);
	}

	@Test
	public void testUpdateComplaintWithQuantity() {
		BigDecimal th = new BigDecimal("10");
		complaint.setTotalLaborHour(new BigDecimal(10));
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.updateComplaintWithQuantity(complaint.getId(),th);
	}

	@Test
	public void testUpdateComplaintNullCost() {
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		complaintService.updateComplaintWithCost(complaint.getId(),null);
	}

	@Test
	public void testUpdateComplaintNullQuantity() {
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		complaintService.updateComplaintWithQuantity(complaint.getId(),null);
	}

	@Test
	public void updateComplaintWithCostNullTest() {
		BigDecimal tc = new BigDecimal("0");
		complaint.setTotalSubLetCost(null);
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.updateComplaintWithCost(complaint.getId(),tc);
	}

	@Test
	public void testGetDefaultType() {
		ComplaintCategories types = Struct.create(ComplaintCategories.class);
		types.setCode("F0038");
		types.setName("Alexander");
		when(complaintDao.getDefaultType()).thenReturn(result);
		complaintService.getDefaultType();
	}

	@Test
	public void testGetDefaultTypeResultNotNull() {
		ComplaintCategories types = Struct.create(ComplaintCategories.class);
		types.setCode("F0038");
		types.setName("Alexander");
		List<ComplaintCategories> complaintTypesList = new ArrayList<>();
		complaintTypesList.add(types);
		when(complaintDao.getDefaultType()).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("isRelevant", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.first()).thenReturn(opt);
		when(result.listOf(ComplaintCategories.class)).thenReturn(complaintTypesList);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.getDefaultType();
	}

	@Test
	public void testGetDefaultStatus() {
		ComplaintStatuses statuses = Struct.create(ComplaintStatuses.class);
		statuses.setCode("F0038");
		statuses.setName("Alexander");
		when(complaintDao.getDefaultStatus()).thenReturn(result);
		complaintService.getDefaultStatus();
	}

	@Test
	public void testGetDefaultStatusResultNotNull() {
		ComplaintStatuses statuses = Struct.create(ComplaintStatuses.class);
		statuses.setCode("F0038");
		statuses.setName("Alexander");
		List<ComplaintStatuses> complaintStatusesList = new ArrayList<>();
		complaintStatusesList.add(statuses);
		when(complaintDao.getDefaultStatus()).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("isRelevant", "ComplaintID");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.first()).thenReturn(opt);
		when(result.listOf(ComplaintStatuses.class)).thenReturn(complaintStatusesList);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.getDefaultStatus();
	}

	@Test
	public void testGetComplaintDetails() {
		row.put("Complaint", complaint);
		opt = Optional.of(row);
		when(result.listOf(Complaints.class)).thenReturn(complaintList);
		when(result.first()).thenReturn(opt);
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		Complaints complaintDetails = complaintService.getComplaintDetails(complaint.getId());
	}

	@Test
	public void testGetAllComplaints() {
		when(complaintDao.getAllComplaints()).thenReturn(result);
		complaintService.getAllComplaints();
	}

	@Test
	public void getActiveComplaintsForBusinessPartnerTest() {
		Complaints statuses = Struct.create(Complaints.class);
		BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
		List<String> businessPartnerList = new ArrayList<>();
		statuses.setSupplierId("11111");
		statuses.setContactPersonId("23121");
		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "100");
		row.put("name", "F001");
		row.put("descr", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(complaintDao.getActiveComplaintsForSupplier(any(String.class))).thenReturn(result);
		complaintService.getActiveComplaintsForBusinessPartner(statuses, businessPartners, businessPartnerList);
	}

	@Test
	public void testGetActiveComplaintsForBusinessPartnerListNotEmpty() {
		Complaints statuses = Struct.create(Complaints.class);
		BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
		List<String> businessPartnerList = new ArrayList<>();
		statuses.setSupplierId("11111");
		statuses.setContactPersonId("23121");
		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "100");
		row.put("name", "F001");
		row.put("descr", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(result.listOf(Complaints.class)).thenReturn(complaintList);
		when(complaintDao.getActiveComplaintsForSupplier(any(String.class))).thenReturn(result);
		complaintService.getActiveComplaintsForBusinessPartner(statuses, businessPartners, businessPartnerList);
	}

	@Test
	public void getActiveComplaintsForBusinessPartnerNullTest() {
		Complaints statuses = Struct.create(Complaints.class);
		BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
		List<String> businessPartnerList = new ArrayList<>();
		statuses.setSupplierId("11111");
		statuses.setContactPersonId("23121");
		when(complaintDao.getActiveComplaintsForSupplier(any(String.class))).thenReturn(null);
		complaintService.getActiveComplaintsForBusinessPartner(statuses, businessPartners, businessPartnerList);
	}

	@Test
	public void checkActiveComplaintsForDeleteMasterDataTest() {
		Complaints statuses = Struct.create(Complaints.class);
		List<String> businessPartnerList = new ArrayList<>();
		BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
		businessPartners.setBusinessPartnerNumber("SUPPLIER");
		businessPartners.setId("11111");
		businessPartners.setIsMarkedForDeletion(true);
		BusinessPartners contactPerson = Struct.create(BusinessPartners.class);
		contactPerson.setBusinessPartnerNumber("AJAY");
		contactPerson.setId("23121");
		contactPerson.setIsMarkedForDeletion(true);
		statuses.setSupplierId("11111");
		statuses.setContactPersonId("23121");
		businessPartnerList.add("11111");
		businessPartnerList.add("23121");
		when(complaintDao.getActiveComplaintsForSupplier(any(String.class))).thenReturn(result);
		when(configurationService.getSupplier("11111")).thenReturn(businessPartners);
		when(configurationService.getSupplier("23121")).thenReturn(contactPerson);
		doNothing().when(businessPartnerRepository).deleteBusinessPartnerList(businessPartnerList);
		complaintService.checkActiveComplaintsForDeleteMasterData(statuses);
	}

	@Test
	public void testCheckActiveComplaintsForDeleteMasterDataNull1() {
		complaint.setSupplierId(null);
		complaint.setContactPersonId(null);
		complaintService.checkActiveComplaintsForDeleteMasterData(complaint);
	}

	@Test
	public void testCheckActiveComplaintsForDeleteMasterDataNull() {
		Complaints statuses = Struct.create(Complaints.class);
		List<String> businessPartnerList = new ArrayList<>();
		BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
		businessPartners.setBusinessPartnerNumber("SUPPLIER");
		businessPartners.setId("11111");
		businessPartners.setIsMarkedForDeletion(true);
		BusinessPartners contactPerson = Struct.create(BusinessPartners.class);
		contactPerson.setBusinessPartnerNumber("AJAY");
		contactPerson.setId("23121");
		contactPerson.setIsMarkedForDeletion(true);
		statuses.setSupplierId(null);
		statuses.setContactPersonId(null);
		businessPartnerList.add("11111");
		businessPartnerList.add("23121");
		when(complaintDao.getActiveComplaintsForSupplier(any(String.class))).thenReturn(result);
		when(configurationService.getSupplier("11111")).thenReturn(businessPartners);
		when(configurationService.getSupplier("23121")).thenReturn(contactPerson);
		doNothing().when(businessPartnerRepository).deleteBusinessPartnerList(businessPartnerList);
		complaintService.checkActiveComplaintsForDeleteMasterData(statuses);
	}
	@Test
	public void testValidateComplaintAttributes() {
		complaint.setSupplierId("1111");
		complaint.setMaterialId("2222");
		complaint.setPlantId("3333");
		complaint.setQuantity(new BigDecimal(1));
		complaint.setUnitCode("ST");
		complaint.setPurchasingOrganizationId("4444");
		complaint.setCompanyCodeId("5555");
		complaintService.validateComplaintAttributes(complaint);
	}
	@Test
	public void testUpdateComplaintWithQuantityNull() {
		BigDecimal th = new BigDecimal("0");
		complaint.setTotalLaborHour(new BigDecimal("0"));
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		when(result.single(Complaints.class)).thenReturn(complaint);
		complaintService.updateComplaintWithQuantity(complaint.getId(),th);
	}

	@Test
	public void testUpdateComplaintWithQuantityTotalHourNull() {
		BigDecimal th = new BigDecimal("0");
		complaint.setTotalLaborHour(null);
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "100");
		row.put("name", "F001");
		row.put("descr", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(result.single(Complaints.class)).thenReturn(complaint);
		when(result.listOf(Complaints.class)).thenReturn(complaintList);
		complaintService.updateComplaintWithQuantity(complaint.getId(),th);
	}

	@Test
	public void checkActiveComplaintsForDeleteMasterDataNullTest() {
		Complaints statuses = Struct.create(Complaints.class);
		List<String> businessPartnerList = new ArrayList<>();
		BusinessPartners businessPartners = Struct.create(BusinessPartners.class);
		businessPartners.setBusinessPartnerNumber("SUPPLIER");
		businessPartners.setId("11111");
		businessPartners.setIsMarkedForDeletion(true);
		BusinessPartners contactPerson = Struct.create(BusinessPartners.class);
		contactPerson.setBusinessPartnerNumber("AJAY");
		contactPerson.setId("23121");
		contactPerson.setIsMarkedForDeletion(true);
		statuses.setSupplierId(null);
		statuses.setContactPersonId(null);
		businessPartnerList.add("11111");
		businessPartnerList.add("23121");
		when(complaintDao.getActiveComplaintsForSupplier(any(String.class))).thenReturn(result);
		when(configurationService.getSupplier("11111")).thenReturn(businessPartners);
		when(configurationService.getSupplier("23121")).thenReturn(contactPerson);
		doNothing().when(businessPartnerRepository).deleteBusinessPartnerList(businessPartnerList);
		complaintService.checkActiveComplaintsForDeleteMasterData(statuses);
	}

	@Test
	public void testGetAllResponsiblePerson() {
		//when(accessor.getDestination("BTP-USERS")).thenReturn(destination);
		//when(destination.isHttp()).thenReturn(true);
		complaintService.getAllResponsiblePerson();
	}

	@Test
	public void testDeleteBusinessPartnerWhenComplaintClosed() {
		CommonBusinessObjects object = Struct.create(CommonBusinessObjects.class);
		List<CommonBusinessObjects> businessObjects = new ArrayList<>();
		businessObjects.add(object);
		complaint.setSupplierId("1234");
		complaint.setContactPersonId("1234");
		when(businessPartnerService.checkIsMarkedForDeletion("1234")).thenReturn(true);
		when(businessPartnerService.businessPartnerUsedByAnyComplaint("1234")).thenReturn(false);
		when(businessPartnerService.businessPartnerUsedByAnyBusinessObject("1234")).thenReturn(false);
		when(businessObjectDao.getCommonBusinessObjectsBasedOnComplaint(complaint.getId())).thenReturn(businessObjects);

		complaintService.deleteBusinessPartnerWhenComplaintClosed(complaint);
	}

	@Test
	public void testDeleteBusinessPartnerWhenComplaintClosedSupplierId() {
		CommonBusinessObjects object = Struct.create(CommonBusinessObjects.class);
		object.setSupplierId("1234");
		object.setContactPersonId("1234");
		object.setPersonResponsibleId("1234");
		List<CommonBusinessObjects> businessObjects = new ArrayList<>();
		businessObjects.add(object);
		complaint.setSupplierId("1234");
		complaint.setContactPersonId("1234");
		when(businessPartnerService.checkIsMarkedForDeletion("1234")).thenReturn(true);
		when(businessPartnerService.businessPartnerUsedByAnyComplaint("1234")).thenReturn(false);
		when(businessPartnerService.businessPartnerUsedByAnyBusinessObject("1234")).thenReturn(false);
		when(businessObjectDao.getCommonBusinessObjectsBasedOnComplaint(complaint.getId())).thenReturn(businessObjects);

		complaintService.deleteBusinessPartnerWhenComplaintClosed(complaint);
	}

	@Test
	public void testDeleteBusinessPartnerWhenComplaintClosed1() {
		CommonBusinessObjects object = Struct.create(CommonBusinessObjects.class);
		List<CommonBusinessObjects> businessObjects = new ArrayList<>();
		businessObjects.add(object);
		complaint.setSupplierId("1234");
		complaint.setContactPersonId("1234");
		when(businessPartnerService.checkIsMarkedForDeletion("1234")).thenReturn(false);
		when(businessPartnerService.businessPartnerUsedByAnyComplaint("1234")).thenReturn(true);
		when(businessPartnerService.businessPartnerUsedByAnyBusinessObject("1234")).thenReturn(true);
		when(businessObjectDao.getCommonBusinessObjectsBasedOnComplaint(complaint.getId())).thenReturn(businessObjects);
		complaintService.deleteBusinessPartnerWhenComplaintClosed(complaint);
	}

	@Test
	public void testGetIsComplaintStatusClosedBasedOnBusinessPartner() {
		when(complaintDao.getIsComplaintStatusClosedBasedOnSupplier("1234")).thenReturn(true);
		when(complaintDao.getIsComplaintStatusClosedBasedOnContactPerson("1234")).thenReturn(true);
		complaintService.getIsComplaintStatusClosedBasedOnBusinessPartner("1234");
		assertEquals(true, complaintService.getIsComplaintStatusClosedBasedOnBusinessPartner("1234"));
	}

	@Test
	public void testDeleteComplaints() {
		complaintService.deleteComplaints(complaint.getId());
	}
    
    @Test
    public void testGetMasterDataFromComplaints(){
        complaint.setSupplierId("12");
        complaint.setPlantId("22");
        complaint.setMaterialId("13");
        complaint.setCompanyCodeId("11");
        complaint.setQuantity(new BigDecimal(1));
        complaint.setUnitCode("ST");
        complaint.setPurchasingOrganizationId("33");
        List<Complaints> compList = new ArrayList<>();
        compList.add(complaint);
        row.put("Complaint", complaint);
		opt = Optional.of(row);
		when(result.listOf(Complaints.class)).thenReturn(compList);
		when(result.first()).thenReturn(opt);
		when(complaintDao.getMasterDataFromComplaints(complaint.getId())).thenReturn(result);
        complaintService.getMasterDataFromComplaints(complaint.getId());
    }
   
    @Test
    public void testGetComplaintCreationType(){
        
        List<Row> rowvalues = new ArrayList<>();
		row.put("creationType", "Automatic");
		row.put("referenceNumber", "123");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Complaints.class)).thenReturn(complaintList);
		when(result.first()).thenReturn(opt);
        when(complaintDao.getComplaintCreationTypeAndCompanyCode(complaint.getId())).thenReturn(result);
       complaintService.getComplaintCreationTypeAndCompanyCode(complaint.getId());
    }

	@Test
	public void testValidateComplaintStatus(){
		complaint.setComplaintStatusCode(Constants.COMPLAINT_DISCARDED);
		row.put("Complaint", complaint);
		opt = Optional.of(row);
		when(result.listOf(Complaints.class)).thenReturn(complaintList);
		when(result.first()).thenReturn(opt);
		when(complaintDao.getComplaintDetails(complaint.getId())).thenReturn(result);
		complaintService.validateComplaintStatus(complaint.getId());
	}
    
}