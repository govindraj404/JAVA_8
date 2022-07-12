package com.sap.ic.cmh.masterdata.businesspartner.service;


import cds.gen.complaintservice.CommonBusinessObjects;
import cds.gen.complaintservice.Complaints;
import cds.gen.masterdataservice.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerRequest;
import com.sap.ic.cmh.masterdata.businesspartner.model.BusinessPartnerResponse;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepositoryImpl;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class BusinessPartnerServiceImplTest  {
	
	@Autowired
    @InjectMocks
    BusinessPartnerServiceImpl businessPartnerService;

    @Mock
    BusinessPartnerRepository businessPartnerRepository;

    @Mock
    LocaleMessageHelper messageHelper;
   @Mock
    Result result;
    @Mock
     PersistenceService db;
    @Mock
     AuditLogHelper auditLogHelper;
    @Mock
    ComplaintService complaintService;
    @Mock
    BusinessObjectDao businessObjectDao;

    private  Map<String, String> businessPartnerDb;
    private  List<String> businessPartnerKeys;
    private  Map<String, String> businessPartnerMap;
    private  List<BusinessPartnerRequest> businessPartnerLists;

    private static String id = null;


    @Before
    public void beforeClass() throws JsonProcessingException{
        MockitoAnnotations.openMocks(this);
        id = UUID.randomUUID().toString();
        businessPartnerKeys = new ArrayList<>();
        businessPartnerKeys.add("10000001");

        businessPartnerMap = new HashMap<>();
        businessPartnerMap.put("10000001", "4052c63c-6351-4ea5-8192-4af6686bb526");

        String json = "[{\"businessPartnerNumber\":\"10000001\"}]";
        businessPartnerLists = new ObjectMapper().readValue(json, new TypeReference<List<BusinessPartnerRequest>>() {
        });
    }
    @Test
    public void testDeleteBusinessPartnerList() {
        businessPartnerDb = new HashMap<>();
        businessPartnerDb.put(id, "4052c63c-6351-4ea5-8192-4af6686bb526");
        when(businessPartnerRepository.getBusinessPartnersMap(businessPartnerKeys)).thenReturn(businessPartnerDb);
        List<String> businessPartnerList = new ArrayList<>(businessPartnerDb.keySet());
        Mockito.doNothing().when(businessPartnerRepository).updateBusinessPartner(
                Matchers.any(BusinessPartners.class));
        List<BusinessPartnerResponse> businessPartnerResponses = businessPartnerService.deleteBusinessPartners(businessPartnerLists);
        Assert.assertEquals(businessPartnerResponses.size(), 2);
    }
    @Test
    public void testUpdateBusinessParter() {
    	  Mockito.doNothing().when(businessPartnerRepository).updateBusinessPartner(
                  Matchers.any(BusinessPartners.class));
        businessPartnerService.updateBusinessPrtnerMarkedForDeletion("");
      
    }

    @Test
    public void businessPartnerUsedByAnyComplaintTest() {
        when(complaintService.getIsComplaintStatusClosedBasedOnBusinessPartner("")).thenReturn(true);
        businessPartnerService.businessPartnerUsedByAnyComplaint("");
    }
    @Test(expected = Exception.class)
    public void businessPartnerUsedByAnyBusinessObjectTest() {
        CommonBusinessObjects commonBusinessObjects= Struct.create(CommonBusinessObjects.class);
        Complaints complaints=Struct.create(Complaints.class);
        complaints.setComplaintStatusCode("CLSD");
        Map<String,Complaints> map=new HashMap<>();
        map.put("complaint",complaints);
        commonBusinessObjects.setComplaint(map);
        complaints=commonBusinessObjects.getComplaint();
        List<CommonBusinessObjects> list=new ArrayList<>();
        list.add(commonBusinessObjects);
        when(businessObjectDao.getCommonBusinessObjectsBasedOnBusinessPartner("")).thenReturn(list);
        businessPartnerService.businessPartnerUsedByAnyBusinessObject("");
    }

    @Test
    public void checkIsMarkedForDeletionNullTest(){
        when(businessPartnerRepository.getBusinessPartners(any(String.class))).thenReturn(result);
        businessPartnerService.checkIsMarkedForDeletion("100");
    }

    @Test
    public void checkIsMarkedForDeletionTest(){
        List<BusinessPartners> materialMasterGeneralDatasList = new ArrayList<>();
        BusinessPartners masterGeneralDatas =  Struct.create(BusinessPartners.class);
        masterGeneralDatas.setId("100");
        masterGeneralDatas.setCompanyCode("F001");
        masterGeneralDatas.setIsMarkedForDeletion(true);
        materialMasterGeneralDatasList.add(masterGeneralDatas);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(materialMasterGeneralDatasList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("isMarkedForDeletion", true);
        Optional<BusinessPartners> oft=Optional.of(masterGeneralDatas);
        when(result.first()).thenReturn(Optional.of(row));
        rowvalues.add(row);

        when(result.first(BusinessPartners.class)).thenReturn(oft);
        when(businessPartnerRepository.getBusinessPartners(any(String.class))).thenReturn(result);
        businessPartnerService.checkIsMarkedForDeletion("100");
    }

    @Test
    public void checkIsMarkedForDeletionElseTest(){
        List<BusinessPartners> materialMasterGeneralDatasList = new ArrayList<>();
        BusinessPartners masterGeneralDatas =  Struct.create(BusinessPartners.class);
        masterGeneralDatas.setId("100");
        masterGeneralDatas.setCompanyCode("F001");
        masterGeneralDatas.setIsMarkedForDeletion(true);
        materialMasterGeneralDatasList.add(masterGeneralDatas);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(materialMasterGeneralDatasList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("isMarkedForDeletion", false);
        Optional<BusinessPartners> oft=Optional.of(masterGeneralDatas);
       // when(result.first()).thenReturn(Optional.of(row));
        rowvalues.add(row);

        when(result.first(BusinessPartners.class)).thenReturn(oft);
        when(businessPartnerRepository.getBusinessPartners(any(String.class))).thenReturn(result);
        businessPartnerService.checkIsMarkedForDeletion("100");
    }
    @Test
    public void testDeleteBusinessPartnerElseList() {
        businessPartnerDb = new HashMap<>();
        businessPartnerDb.put(id, "4052c63c-6351-4ea5-8192-4af6686bb526");
        when(businessPartnerRepository.getBusinessPartnersMap(businessPartnerKeys)).thenReturn(businessPartnerDb);
       List<String> businessPartnerList = new ArrayList<>(businessPartnerDb.keySet());
        Mockito.doNothing().when(businessPartnerRepository).updateBusinessPartner(
                Matchers.any(BusinessPartners.class));
        List<String> list=new ArrayList<>();
        list.add("8bfa34ae-917c-45db-a933-f92e2902b90a");
        when(businessPartnerRepository.getActiveComplaintsInBusinessPartner(any(List.class))).thenReturn(list);
         businessPartnerService.deleteBusinessPartners(businessPartnerLists);

    }

    @Test
    public void testDeleteBusinessPartnerElse1List() {
        businessPartnerDb = new HashMap<>();
        businessPartnerDb.put(id, "8bfa34ae-917c-45db-a933-f92e2902b90a");
        when(businessPartnerRepository.getBusinessPartnersMap(businessPartnerKeys)).thenReturn(businessPartnerDb);
        List<String> businessPartnerList = new ArrayList<>(businessPartnerDb.keySet());
        Mockito.doNothing().when(businessPartnerRepository).updateBusinessPartner(
                Matchers.any(BusinessPartners.class));
        List<String> list=new ArrayList<>();
        list.add("");
        when(businessPartnerRepository.getActiveComplaintsInBusinessPartner(any(List.class))).thenReturn(list);
        businessPartnerService.deleteBusinessPartners(businessPartnerLists);

    }
    @Test
    public void testGetBusinessPartnersBasedOnNumberEmpty() {
        Optional<BusinessPartners> oft=Optional.empty();
        BusinessPartners businessPartner = Struct.create(BusinessPartners.class);
        businessPartner.setId("1");
        businessPartner.setBusinessPartnerNumber("1111");
        when(businessPartnerRepository.getBusinessPartnersBasedOnNumber("1111")).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(oft);
        businessPartnerService.getBusinessPartnersBasedOnNumber(businessPartner.getBusinessPartnerNumber());
    }



    @Test
    public void testCheckIfVendorCodeExists() {
        BusinessPartners businessPartner = Struct.create(BusinessPartners.class);
        businessPartner.setId("1");
        businessPartner.setVendorCode("1111");
        Optional<BusinessPartners> oft=Optional.of(businessPartner);
        when(businessPartnerRepository.checkIfVendorCodeExists("1111")).thenReturn(result);
        when(result.first(BusinessPartners.class)).thenReturn(oft);
        businessPartnerService.checkIfVendorCodeExists(businessPartner.getVendorCode());

    }
    @Test
    public void testGetBusinessPartnersBasedOnNumber() {
    	BusinessPartners businessPartner = Struct.create(BusinessPartners.class);
    	businessPartner.setId("1");
    	businessPartner.setBusinessPartnerNumber("1111");
    	Optional<BusinessPartners> oft=Optional.of(businessPartner);
    	when(businessPartnerRepository.getBusinessPartnersBasedOnNumber("1111")).thenReturn(result);
    	when(result.first(BusinessPartners.class)).thenReturn(oft);
    	businessPartnerService.getBusinessPartnersBasedOnNumber(businessPartner.getBusinessPartnerNumber());
    }
    @Test
    public void checkIfCustomerCodeExistsTest(){
        List<BusinessPartners> materialMasterGeneralDatasList = new ArrayList<>();
        BusinessPartners masterGeneralDatas =  Struct.create(BusinessPartners.class);
        masterGeneralDatas.setId("100");
        masterGeneralDatas.setCompanyCode("F001");
        masterGeneralDatas.setIsMarkedForDeletion(true);
        materialMasterGeneralDatasList.add(masterGeneralDatas);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(materialMasterGeneralDatasList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("isMarkedForDeletion", true);
        Optional<BusinessPartners> oft=Optional.of(masterGeneralDatas);
        when(result.first()).thenReturn(Optional.of(row));
        rowvalues.add(row);

        when(result.first(BusinessPartners.class)).thenReturn(oft);
        when(businessPartnerRepository.getBusinessPartners(any(String.class))).thenReturn(result);
        businessPartnerService.checkIsMarkedForDeletion("100");
    }

    @Test
    public void checkIfCustomerCodeExistsElseTest(){
        List<BusinessPartners> materialMasterGeneralDatasList = new ArrayList<>();
        BusinessPartners masterGeneralDatas =  Struct.create(BusinessPartners.class);
        masterGeneralDatas.setId("100");
        masterGeneralDatas.setCompanyCode("F001");
        masterGeneralDatas.setIsMarkedForDeletion(true);
        materialMasterGeneralDatasList.add(masterGeneralDatas);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(materialMasterGeneralDatasList);
        List<Row> rowvalues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "100");
        row.put("isMarkedForDeletion", false);
        Optional<BusinessPartners> oft=Optional.of(masterGeneralDatas);
        // when(result.first()).thenReturn(Optional.of(row));
        rowvalues.add(row);

        when(result.first(BusinessPartners.class)).thenReturn(oft);
        when(businessPartnerRepository.getBusinessPartners(any(String.class))).thenReturn(result);
        businessPartnerService.checkIsMarkedForDeletion("100");
    }
}
