package com.sap.ic.cmh.drm.service;

import cds.gen.masterdataservice.BusinessPartners;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.impl.ResultImpl;
import com.sap.cds.impl.RowImpl;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.drm.model.*;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.companycode.repository.CompanyCodeRepository;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.text.ParseException;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class RetentionManagerServiceImplTest {

    @InjectMocks
    @Autowired
    RetentionManagerServiceImpl impl;

    @Mock
    PersistenceService persistenceService;

    @Mock
    ConfigurationDao configDao;

    @Mock
    ComplaintsDao complaintsDao;

    @Mock
    BusinessPartnerService businessPartnerService;

    @Mock
    Result result;
    
    @Mock
    CompanyCodeRepository companyCodeRepository;

    @Mock
    BusinessPartnerRepository businessPartnerRepository;

    private Row row;
    private Optional<Row> opt;

    BusinessPartners businessPartners;

    List<BusinessPartners> businessPartnersList;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        row = Struct.create(Row.class);
        businessPartners= Struct.create(BusinessPartners.class);
        businessPartners.setBusinessPartnerNumber("1234");
        businessPartners.setBusinessPartnerType("Claim");

        businessPartnersList = new ArrayList<>();
        businessPartnersList.add(businessPartners);
    }

    @Test
    public void testGetLegalEntities() {
        Map<String,Object> map = new HashMap<>();
        map.put("personResponsible_ID", "2356643");
        Map<String,Object> companyCodeMap = new HashMap<>();
        companyCodeMap.put("ID", "122");
        companyCodeMap.put("companyCode", "1000");
        map.put("companyCode", companyCodeMap);
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        impl.getLegalEntities( "BTP Users") ;
    }
    @Test
    public void testGetLegalEntitiesOther() {
        Map<String,Object> map = new HashMap<>();
        map.put("businessPartnerNumber", "1234");
        map.put("businessPartnerId", "2356643");
        map.put("contactPerson_ID" , "1234");
        map.put("supplier_ID" , "1234");
        map.put("personResponsible_ID", "2356643");
        map.put("ID", "1234");
        Map<String,Object> businessPartnerMap = new HashMap<>();
        businessPartnerMap.put("ID", "123");
        businessPartnerMap.put("businessPartnerNumber", "1234");
        Map<String,Object> companyCodeMap = new HashMap<>();
        companyCodeMap.put("ID", "122");
        companyCodeMap.put("companyCode", "1000");
        businessPartnerMap.put("companyCodeID", companyCodeMap);
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        impl.getLegalEntities( "Business Partner") ;
    }
    @Test
    public void testGetLegalEntitiesOtherNull() {
        impl.getLegalEntities("");

    }

    @Test
    public void testGetDataSubjectEndOfResidence(){
        ResidenceRuleConditionSet set = new ResidenceRuleConditionSet();
        set.setResidenceDate("ResidenceDate");
        List<ResidenceRuleConditionSet> ruleConditionSet = new ArrayList<>();
        ruleConditionSet.add(set);
        LegalGroundResidenceRules rules = new LegalGroundResidenceRules();
        rules.setLegalEntity("Complaints");
        rules.setRuleConditionSet(ruleConditionSet);
        List<LegalGroundResidenceRules> legalGroundResidenceRules = new ArrayList<>();
        legalGroundResidenceRules.add(rules);
        DataSubjectsEndofResidenceRequest dataSubjectsEndofResidenceRequest = new DataSubjectsEndofResidenceRequest();
        dataSubjectsEndofResidenceRequest.setLegalGroundResidenceRules(legalGroundResidenceRules);
        dataSubjectsEndofResidenceRequest.setLegalGround("Complaints");
        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        map.put("modifiedAt", "2021-09-09");
        map.put("companyCode_ID", "122");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        when(companyCodeRepository.getCompanyCodeIdBasedOnCode("1000")).thenReturn("122");
        impl.getDataSubjectEndOfResidence(dataSubjectsEndofResidenceRequest);
    }

    @Test
    public void testGetDataSubjectLegalGroundBP(){
        ResidenceRuleConditionSet set = new ResidenceRuleConditionSet();
        set.setResidenceDate("2021-09-09");
        List<ResidenceRuleConditionSet> ruleConditionSet = new ArrayList<>();
        ruleConditionSet.add(set);
        LegalGroundResidenceRules rules = new LegalGroundResidenceRules();
        rules.setLegalEntity("Complaints");
        rules.setRuleConditionSet(ruleConditionSet);
        List<LegalGroundResidenceRules> legalGroundResidenceRules = new ArrayList<>();
        legalGroundResidenceRules.add(rules);
        DataSubjectsEndofResidenceRequest dataSubjectsEndofResidenceRequest = new DataSubjectsEndofResidenceRequest();
        dataSubjectsEndofResidenceRequest.setLegalGroundResidenceRules(legalGroundResidenceRules);
        dataSubjectsEndofResidenceRequest.setLegalGround("Complaints and Business Objects");
        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("businessPartnerNumber", "1234");
        map.put("businessPartnerId", "2356643");
        map.put("supplier_ID" , "1234");
        map.put("contactPerson_ID" , "1234");
        map.put("personResponsible_ID", "2356643");
        map.put("ID", "1234");
        map.put("modifiedAt", "2021-09-09");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        impl.getDataSubjectEndOfResidence(dataSubjectsEndofResidenceRequest);
    }

    @Test
    public void testGetDataSubjectLegalGroundOthers(){
        ResidenceRuleConditionSet set = new ResidenceRuleConditionSet();
        set.setResidenceDate("ResidenceDate");
        List<ResidenceRuleConditionSet> ruleConditionSet = new ArrayList<>();
        ruleConditionSet.add(set);
        LegalGroundResidenceRules rules = new LegalGroundResidenceRules();
        rules.setLegalEntity("Complaints");
        rules.setRuleConditionSet(ruleConditionSet);
        List<LegalGroundResidenceRules> legalGroundResidenceRules = new ArrayList<>();
        legalGroundResidenceRules.add(rules);
        DataSubjectsEndofResidenceRequest dataSubjectsEndofResidenceRequest = new DataSubjectsEndofResidenceRequest();
        dataSubjectsEndofResidenceRequest.setLegalGroundResidenceRules(legalGroundResidenceRules);
        dataSubjectsEndofResidenceRequest.setLegalGround("gdhshf");
        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);

        impl.getDataSubjectEndOfResidence(dataSubjectsEndofResidenceRequest);
    }

    @Test
    public void testGetDataSubjectEndOfResidenceElse(){
        List<LegalGroundResidenceRules> legalGroundResidenceRules = new ArrayList<>();
        legalGroundResidenceRules.add(null);
        DataSubjectsEndofResidenceRequest dataSubjectsEndofResidenceRequest = new DataSubjectsEndofResidenceRequest();
        dataSubjectsEndofResidenceRequest.setLegalGround("legalGround");
        dataSubjectsEndofResidenceRequest.setLegalGroundResidenceRules(legalGroundResidenceRules);
        impl.getDataSubjectEndOfResidence(dataSubjectsEndofResidenceRequest);
    }

    @Test
    public void testGetDataSubjectLastRetentionStartDates() {
        RulesCondition rulesCondition = new RulesCondition();
        rulesCondition.setRetentionID("244");
        List<RulesCondition> rulesConditionList = new ArrayList<>();
        rulesConditionList.add(rulesCondition);
        DataSubjectLastRetentionStartDatesRequest dataSubjectLastRetentionStartDatesRequest =
                new DataSubjectLastRetentionStartDatesRequest();
        dataSubjectLastRetentionStartDatesRequest.setDataSubjectID("1234");
        dataSubjectLastRetentionStartDatesRequest.setDataSubjectRole("Role");
        dataSubjectLastRetentionStartDatesRequest.setStartTime("2021-09-09");
        dataSubjectLastRetentionStartDatesRequest.setLegalEntity("Complaints");
        dataSubjectLastRetentionStartDatesRequest.setLegalGround("Business Partners");
        dataSubjectLastRetentionStartDatesRequest.setRulesConditionSet(rulesConditionList);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        map.put("modifiedAt", "2021-09-09");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        impl.getDataSubjectLastRetentionStartDates(dataSubjectLastRetentionStartDatesRequest);
    }

    @Test
    public void testGetDataSubjectLastRetentionStartDatesParseException() {
        RulesCondition rulesCondition = new RulesCondition();
        rulesCondition.setRetentionID("244");
        List<RulesCondition> rulesConditionList = new ArrayList<>();
        rulesConditionList.add(rulesCondition);
        DataSubjectLastRetentionStartDatesRequest dataSubjectLastRetentionStartDatesRequest =
                new DataSubjectLastRetentionStartDatesRequest();
        dataSubjectLastRetentionStartDatesRequest.setDataSubjectID("1234");
        dataSubjectLastRetentionStartDatesRequest.setDataSubjectRole("Role");
        dataSubjectLastRetentionStartDatesRequest.setStartTime("2021-09-09");
        dataSubjectLastRetentionStartDatesRequest.setLegalEntity("Complaints");
        dataSubjectLastRetentionStartDatesRequest.setLegalGround("Business Partners");
        dataSubjectLastRetentionStartDatesRequest.setRulesConditionSet(rulesConditionList);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "fghhgjh-09-09");
        map.put("personResponsible_ID", "2356643");
        map.put("modifiedAt", "2fg21-09-09");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        impl.getDataSubjectLastRetentionStartDates(dataSubjectLastRetentionStartDatesRequest);
    }

    @Test
    public void testGetDataSubjectLastRetentionStartDatesDataSubjectIdNull() {
        RulesCondition rulesCondition = new RulesCondition();
        rulesCondition.setRetentionID("244");
        List<RulesCondition> rulesConditionList = new ArrayList<>();
        rulesConditionList.add(rulesCondition);
        DataSubjectLastRetentionStartDatesRequest dataSubjectLastRetentionStartDatesRequest =
                new DataSubjectLastRetentionStartDatesRequest();
        dataSubjectLastRetentionStartDatesRequest.setDataSubjectID(null);
        dataSubjectLastRetentionStartDatesRequest.setDataSubjectRole("Role");
        dataSubjectLastRetentionStartDatesRequest.setStartTime("2021-09-09");
        dataSubjectLastRetentionStartDatesRequest.setLegalEntity("Complaints");
        dataSubjectLastRetentionStartDatesRequest.setLegalGround("Business Partners");
        dataSubjectLastRetentionStartDatesRequest.setRulesConditionSet(rulesConditionList);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        impl.getDataSubjectLastRetentionStartDates(dataSubjectLastRetentionStartDatesRequest);
    }

    @Test
    public void testDeleteDataSubjectCase2() throws ParseException {

        DataSubjectRequest dataSubjectRequest = new DataSubjectRequest();
        dataSubjectRequest.setDataSubjectID("5777");
        dataSubjectRequest.setMaxDeletionDate("2021-09-09");
        when(configDao.getSupplierDataBasedOnNumber(dataSubjectRequest.getDataSubjectID())).thenReturn(result);
        List<Row> rowList = new ArrayList<>();
        row.put("personResponsible_ID", "personResponsible_ID");
        opt = Optional.of(row);
        rowList.add(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(complaintsDao.getActiveComplaintsForPersonResponsible(dataSubjectRequest.getDataSubjectID())).thenReturn(result);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        when(persistenceService.run(any(CqnUpdate.class))).thenReturn(result);
        when(businessPartnerService.businessPartnerUsedByAnyComplaint(any())).thenReturn(false);
        when(businessPartnerService.businessPartnerUsedByAnyBusinessObject(any())).thenReturn(false);
        impl.deleteDataSubject(dataSubjectRequest);
    }

    @Test
    public void testDeleteDataSubjectBPIsSUP() throws ParseException {
        businessPartners.setBusinessPartnerType("SUP");
        businessPartnersList.add(businessPartners);
        DataSubjectRequest dataSubjectRequest = new DataSubjectRequest();
        dataSubjectRequest.setDataSubjectID("5777");
        dataSubjectRequest.setMaxDeletionDate("2021-09-09");
        when(configDao.getSupplierDataBasedOnNumber(dataSubjectRequest.getDataSubjectID())).thenReturn(result);
        List<Row> rowList = new ArrayList<>();
        row.put("personResponsible_ID", "personResponsible_ID");
        opt = Optional.of(row);
        rowList.add(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(complaintsDao.getActiveComplaintsForPersonResponsible(dataSubjectRequest.getDataSubjectID())).thenReturn(result);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        when(persistenceService.run(any(CqnUpdate.class))).thenReturn(result);
        when(businessPartnerService.businessPartnerUsedByAnyComplaint(any())).thenReturn(false);
        when(businessPartnerService.businessPartnerUsedByAnyBusinessObject(any())).thenReturn(false);
        impl.deleteDataSubject(dataSubjectRequest);
    }

    @Test
    public void testDeleteDataSubjectBPIsSUPCON() throws ParseException {
        businessPartners.setBusinessPartnerType("SUPCON");
        businessPartnersList.add(businessPartners);
        DataSubjectRequest dataSubjectRequest = new DataSubjectRequest();
        dataSubjectRequest.setDataSubjectID("5777");
        dataSubjectRequest.setMaxDeletionDate("2021-09-09");
        when(configDao.getSupplierDataBasedOnNumber(dataSubjectRequest.getDataSubjectID())).thenReturn(result);
        List<Row> rowList = new ArrayList<>();
        row.put("personResponsible_ID", "personResponsible_ID");
        opt = Optional.of(row);
        rowList.add(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(complaintsDao.getActiveComplaintsForPersonResponsible(dataSubjectRequest.getDataSubjectID())).thenReturn(result);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        when(persistenceService.run(any(CqnUpdate.class))).thenReturn(result);
        when(businessPartnerService.businessPartnerUsedByAnyComplaint(any())).thenReturn(false);
        when(businessPartnerService.businessPartnerUsedByAnyBusinessObject(any())).thenReturn(false);
        impl.deleteDataSubject(dataSubjectRequest);
    }

    @Test
    public void testDeleteDataSubjectBPIsPERRES() throws ParseException {
        businessPartners.setBusinessPartnerType("PERRES");
        businessPartnersList.add(businessPartners);
        DataSubjectRequest dataSubjectRequest = new DataSubjectRequest();
        dataSubjectRequest.setDataSubjectID("5777");
        dataSubjectRequest.setMaxDeletionDate("2021-09-09");
        when(configDao.getSupplierDataBasedOnNumber(dataSubjectRequest.getDataSubjectID())).thenReturn(result);
        List<Row> rowList = new ArrayList<>();
        row.put("personResponsible_ID", "personResponsible_ID");
        opt = Optional.of(row);
        rowList.add(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(complaintsDao.getActiveComplaintsForPersonResponsible(dataSubjectRequest.getDataSubjectID())).thenReturn(result);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        when(persistenceService.run(any(CqnUpdate.class))).thenReturn(result);
        when(businessPartnerService.businessPartnerUsedByAnyComplaint(any())).thenReturn(false);
        when(businessPartnerService.businessPartnerUsedByAnyBusinessObject(any())).thenReturn(false);
        impl.deleteDataSubject(dataSubjectRequest);
    }

    @Test
    public void testDeleteDataSubject() throws ParseException {

        DataSubjectRequest dataSubjectRequest = new DataSubjectRequest();
        dataSubjectRequest.setDataSubjectID("5777");
        dataSubjectRequest.setMaxDeletionDate("2021-09-09");
        when(configDao.getSupplierDataBasedOnNumber(dataSubjectRequest.getDataSubjectID())).thenReturn(result);
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(complaintsDao.getActiveComplaintsForPersonResponsible(dataSubjectRequest.getDataSubjectID())).thenReturn(result);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        when(persistenceService.run(any(CqnUpdate.class))).thenReturn(result);
        impl.deleteDataSubject(dataSubjectRequest);
    }

    @Test
    public void testDeleteDataSubjectNull() throws ParseException {

        DataSubjectRequest dataSubjectRequest = new DataSubjectRequest();
        dataSubjectRequest.setDataSubjectID(null);
        dataSubjectRequest.setMaxDeletionDate("2021-09-09");
        when(configDao.getSupplierDataBasedOnNumber(dataSubjectRequest.getDataSubjectID())).thenReturn(result);
        List<Row> rowList = new ArrayList<>();
        row.put("personResponsible_ID", "personResponsible_ID");
        opt = Optional.of(row);
        rowList.add(row);
        when(result.first()).thenReturn(opt);
        when(result.listOf(BusinessPartners.class)).thenReturn(businessPartnersList);
        when(complaintsDao.getActiveComplaintsForPersonResponsible(dataSubjectRequest.getDataSubjectID())).thenReturn(result);

        Map<String,Object> map = new HashMap<>();
        map.put("createdAt", "2021-09-09");
        map.put("personResponsible_ID", "2356643");
        Row row = RowImpl.row(map);
        Result result = ResultImpl.insertedRows(Collections.singletonList(row)).result();
        when(persistenceService.run(any(CqnSelect.class))).thenReturn(result);
        when(persistenceService.run(any(CqnUpdate.class))).thenReturn(result);
        when(businessPartnerService.businessPartnerUsedByAnyComplaint(any())).thenReturn(false);
        when(businessPartnerService.businessPartnerUsedByAnyBusinessObject(any())).thenReturn(false);
        impl.deleteDataSubject(dataSubjectRequest);
    }

}
