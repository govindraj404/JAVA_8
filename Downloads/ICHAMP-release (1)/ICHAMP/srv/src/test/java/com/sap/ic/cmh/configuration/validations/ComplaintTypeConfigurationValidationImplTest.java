package com.sap.ic.cmh.configuration.validations;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ItemCategoryService;
import org.junit.Before;
import org.junit.Test;
import org.junit.platform.commons.util.StringUtils;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import com.sap.ic.cmh.utils.SecurityValidator;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;

import cds.gen.configurationservice.ComplaintTypeConfigurations;
import cds.gen.configurationservice.ComplaintTypeToSalesAreaMappings;
import cds.gen.configurationservice.ComplaintTypeToItemCategoryMappings;

public class ComplaintTypeConfigurationValidationImplTest {

    @InjectMocks
    ComplaintTypeConfigurationValidationImpl validator;

    @Mock
    Messages messages;

    @Mock
    SecurityValidator securityValidator;

    @Mock
    ComplaintTypeConfigurationDao complaintTypeDao;

    @Mock
    ItemCategoriesDao complaintItemCategoriesDao;

    @Mock
    SalesOrganizationRepository salesOrganizationRepository;

    @Mock
    DistributionChannelRepository distributionChannelRepository;

    @Mock
    DivisionRepository divisionRepository;

    @Mock
    Result result;

    @Mock
    MasterDataValidation masterDataValidation;

    @Mock
    ItemCategoryService service;
    @Mock
    private Message msg;

    @Mock
    ConfigurationFieldsValidation fieldsValidation;

    @Mock
    DataValidator dataValidator;

    private ComplaintTypeConfigurations complaintType;
    private ComplaintTypeConfigurations complaintType1;
    private ComplaintTypeToSalesAreaMappings complaintTypeToSalesAreaMappings;
    private ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings;
    private ComplaintTypeToItemCategoryMappings complaintTypeToItemCategoryMappings1;
    List<ComplaintTypeToSalesAreaMappings> ComplaintTypeToSalesAreaList = new ArrayList<>();
    List<ComplaintTypeToSalesAreaMappings> ComplaintTypeToSalesAreaList1 = new ArrayList<>();
    List<ComplaintTypeToItemCategoryMappings> complaintTypeToItemCategoryMappingsList = new ArrayList<>();
    List<ComplaintTypeToItemCategoryMappings> complaintTypeToItemCategoryMappingsList1 = new ArrayList<>();
    private Row row;
    private Row row1;
    private Optional<Row> opt;
    @Mock
    ComplaintsDao complaintsDao;

    @Before
    public void setupBefore() {
        MockitoAnnotations.openMocks(this);
        complaintType = Struct.create(ComplaintTypeConfigurations.class);
        complaintType1 = Struct.create(ComplaintTypeConfigurations.class);
        complaintTypeToSalesAreaMappings = Struct.create(ComplaintTypeToSalesAreaMappings.class);
        complaintTypeToItemCategoryMappings = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        complaintTypeToItemCategoryMappings1 = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        complaintTypeToItemCategoryMappings1.setItemCategoryId("123456");
        complaintTypeToItemCategoryMappings.setItemCategoryId("123456");
        complaintTypeToItemCategoryMappings.setComplaintTypeId("123");
        complaintTypeToItemCategoryMappingsList.add(complaintTypeToItemCategoryMappings);
        complaintTypeToItemCategoryMappingsList1.add(complaintTypeToItemCategoryMappings);
        complaintTypeToItemCategoryMappingsList1.add(complaintTypeToItemCategoryMappings1);

        complaintTypeToSalesAreaMappings.setId("789");
        ComplaintTypeToSalesAreaList.add(complaintTypeToSalesAreaMappings);

        complaintType.setCode("CODE");
        complaintType.setComplaintTypeToSalesAreaMappings(ComplaintTypeToSalesAreaList);
        complaintType.setDescription("description");

        row = Struct.create(Row.class);
        row1 = Struct.create(Row.class);
        row.put("code", "description");
        opt = Optional.of(row);

        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
    }

    @Test
    public void testValidateCompliantTypeConfiguration() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(complaintTypeDao.getComplaintTypeConfigurationBasedOnCode(any(String.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        validator.validateCompliantTypeConfiguration(complaintType);
    }

    @Test
    public void testValidateDescription() {
        validator.validateDescription(complaintType);
    }

    @Test
    public void testValidateCodeIsBlank() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        validator.validateCode(complaintType1);
    }

    @Test
    public void testValidateCodeNotBlankAndDifferentId() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(complaintTypeDao.getComplaintTypeConfigurationBasedOnCode(any(String.class))).thenReturn(result);
        row1.put("ID", "101");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<Row> rows = new ArrayList<>();
        rows.add(row1);
        when(result.list()).thenReturn(rows);
        complaintType.setId("100");
        validator.validateCode(complaintType);
    }

    @Test
    public void testValidateCodeNotBlankAndSameId() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(complaintTypeDao.getComplaintTypeConfigurationBasedOnCode(any(String.class))).thenReturn(result);
        row1.put("ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<Row> rows = new ArrayList<>();
        rows.add(row1);
        when(result.list()).thenReturn(rows);
        complaintType.setId("100");
        validator.validateCode(complaintType);
    }

    @Test
    public void testValidateComplaintTypeToSalesAreaMappings() {
        when(fieldsValidation.validateDestination(any(String.class))).thenReturn(false);
        when(fieldsValidation.validateDestinationValue(any(String.class))).thenReturn(false);
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        complaintTypeToSalesAreaMappings.setSalesOrganizationId("12234");
        complaintTypeToSalesAreaMappings.setDistributionChannelId("456");
        complaintTypeToSalesAreaMappings.setDivisionId("123");
        complaintTypeToSalesAreaMappings.setComplaintTypeConfigurationId("1234");
        ComplaintTypeToSalesAreaList.add(complaintTypeToSalesAreaMappings);
        validator.validateComplaintTypeToSalesAreaMappings(ComplaintTypeToSalesAreaList);
    }

    @Test
    public void testValidateComplaintTypeToSalesAreaMappingsIsEmpty() {
        when(fieldsValidation.validateDestination(any(String.class))).thenReturn(false);
        when(fieldsValidation.validateDestinationValue(any(String.class))).thenReturn(false);
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        validator.validateComplaintTypeToSalesAreaMappings(ComplaintTypeToSalesAreaList1);
    }

    @Test
    public void testValidateComplaintCategoryIsBlank() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        validator.validateComplaintCategory(complaintType);
    }

    @Test
    public void testValidateComplaintCategoryIsNotBlankAndValid() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(complaintsDao.getComplaintCategoryBasedOnCode(any(String.class))).thenReturn(result);
        row1.put("COMPLAINT_CATEGORY_CODE", "100");
        row1.put("code", "description");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        complaintType.setCode("code1");
        complaintType.setComplaintCategoryCode("code1");
        validator.validateComplaintCategory(complaintType);
    }

    @Test
    public void testValidateComplaintCategoryIsInvalidCategoryCode() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(complaintsDao.getComplaintCategoryBasedOnCode(any(String.class))).thenReturn(result);
        row1 = Struct.create(Row.class);
        opt = Optional.empty();
        when(result.first()).thenReturn(opt);
        complaintType.setComplaintCategoryCode("code1");
        validator.validateComplaintCategory(complaintType);
    }

    @Test
    public void testValidateDefaultItemCategory() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        validator.validateDefaultItemCategory("id", true);
    }

    @Test
    public void testValidateDefaultItemActiveCategory3() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        when(service.getActive("any")).thenReturn(true);
        validator.validateDefaultItemCategory("id", false);
    }
    @Test
    public void testValidateDefaultItemActiveCategory2() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        when(service.getActive("any")).thenReturn(false);
        validator.validateDefaultItemCategory("id", null);
    }
    @Test
    public void testValidateDefaultItemActiveCategory1() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        when(service.getActive("any")).thenReturn(true);
        validator.validateDefaultItemCategory("id", null);
    }

    @Test
    public void testValidateDefaultItemActiveCategory() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        when(service.getActive("any")).thenReturn(false);
        validator.validateDefaultItemCategory("id", false);
    }
    @Test
    public void testValidateDefaultItemActiveCategory4() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        when(service.getActive("any")).thenReturn(false);
        validator.validateDefaultItemCategory("id", true);
    }

    @Test
    public void testValidateDefaultItemCategoryNull() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        validator.validateDefaultItemCategory(null, null);
    }

    @Test
    public void testValidateWhentItemCategoryIdisNull() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(false);
        validator.validateDefaultItemCategory(null, false);
    }

    @Test
    public void testValidateDefaultItemCategoryNotExist() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(false);
        validator.validateDefaultItemCategory("id", true);
    }

    @Test
    public void testValidateIndividualComplaintType() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(true);
        validator.validateDefaultItemCategory("id", false);
    }


    @Test
    public void testValidateIndividualComplaintTypeWithNullId() {
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(fieldsValidation.checkItemCategoryExist(any(String.class))).thenReturn(false);
        validator.validateDefaultItemCategory(null, true);
    }

    @Test
    public void testValidateUniqueComplaintTypeToSalesAreaMappings() {
        complaintTypeToSalesAreaMappings.setSalesOrganizationId("12234");
        complaintTypeToSalesAreaMappings.setDistributionChannelId("456");
        complaintTypeToSalesAreaMappings.setDivisionId("123");
        complaintTypeToSalesAreaMappings.setComplaintTypeConfigurationId("1234");
        ComplaintTypeToSalesAreaList.add(complaintTypeToSalesAreaMappings);
        validator.validateUniqueComplaintTypeToSalesAreaMappings(ComplaintTypeToSalesAreaList);
    }

}

