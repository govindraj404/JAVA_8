package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ComplaintReasonMappings;
import cds.gen.masterdataservice.DistributionChannels;
import cds.gen.masterdataservice.Divisions;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonMappingsDao;
import com.sap.ic.cmh.configuration.persistency.ComplaintReasonsDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.configuration.service.ComplaintReasonsService;
import com.sap.ic.cmh.configuration.service.ItemCategoryService;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class ComplaintReasonMappingsValidationImplTest {
    @InjectMocks
    ComplaintReasonMappingsValidationImpl complaintReasonMappingsValidation;

    @Mock
    ComplaintReasonMappingsDao complaintReasonMappingsDao;

    @Mock
    DivisionRepository divisionRepository;

    @Mock
    DistributionChannelRepository distributionChannelRepository;

    @Mock
    SalesOrganizationRepository salesOrganizationRepository;

    @Mock
    ItemCategoriesDao itemCategoriesDao;

    @Mock
    ComplaintReasonsDao complaintReasonsDao;

    @Mock
    Messages messages;
    @Mock
    Message message1;

    @Mock
    protected PersistenceService mockDb;
    @Mock
    Result result;
    @Mock
    Result result1;
    @Mock
    ItemCategoryService itemCategoryService;
    @Mock
    ComplaintReasonsService complaintReasonsService;


    private ComplaintReasonMappings complaintReasonMappings;
    private DistributionChannels distributionChannels;
    private Divisions divisions;


    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);
        complaintReasonMappings = Struct.create(ComplaintReasonMappings.class);
        distributionChannels = Struct.create(DistributionChannels.class);
        divisions = Struct.create(Divisions.class);
        complaintReasonMappings.setSalesOrganizationId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDistributionChannelId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDivisionId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setItemCategoryId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setComplaintReasonId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateSalesItemReasonTestExist() {
        when(complaintReasonMappingsDao.getComplaintReasonMapBasedOnAttributeIDs(
                any(String.class), any(String.class), any(String.class), any(String.class),
                any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.validateComplaintReasonMapExistWithAttributeIds(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryExist() {
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(itemCategoriesDao
                .getComplaintItemCategory("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        complaintReasonMappingsValidation.checkItemCategoryExist(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCheckSalesOrgIDNullCheck() {
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        when(itemCategoriesDao
                .getComplaintItemCategory("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        complaintReasonMappingsValidation.checkSalesOrganizationById(
                null);
    }

    @Test
    public void beforeCreateReasonCategoryExist() {
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        when(complaintReasonsDao
                .getComplaintReasonBasedOnID("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        complaintReasonMappingsValidation.checkComplaintReasonExist(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateReasonCategoryExistNull() {
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(complaintReasonsDao
                .getComplaintReasonBasedOnID("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        complaintReasonMappingsValidation.checkComplaintReasonExist(
                null);
    }

    @Test
    public void beforeCreateItemCategorySalesOrgIDExist() {
        when(salesOrganizationRepository.getSalesOrganizationById(
                any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkSalesOrganizationById(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategorySalesOrgIDExistBlank() {
        when(salesOrganizationRepository.getSalesOrganizationById(
                any(String.class))).thenReturn(result);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkSalesOrganizationById(
                "");
    }

    @Test
    public void beforeCreateItemCategoryDivisionExist() {
        when(divisionRepository.getDivisionIDandSalesIDById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        List<Divisions> divisionsList = new ArrayList<>();
        divisions.setId("ab12");
        divisions.setSalesOrganizationIDId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af0");
        divisionsList.add(divisions);
        when(result.listOf(Divisions.class)).thenReturn(divisionsList);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDivisionExistAndIsMappedWithSalesID(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDistributionExist() {
        when(distributionChannelRepository.getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        List<DistributionChannels> distributionChannelsList = new ArrayList<>();
        distributionChannels.setId("ab12");
        distributionChannels.setSalesOrganizationIDId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af0");
        distributionChannelsList.add(distributionChannels);
        when(result.listOf(DistributionChannels.class)).thenReturn(distributionChannelsList);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDistributionChannelExistAndIsMappedWithSalesID(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDistributionNull() {
        when(distributionChannelRepository.getDistributionChannelById(
                any(String.class))).thenReturn(result);
        when(distributionChannelRepository.getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);

        complaintReasonMappingsValidation.checkDistributionChannelExistAndIsMappedWithSalesID(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategorySalesIDNull() {
        when(salesOrganizationRepository.getSalesOrganizationById(
                any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);

        complaintReasonMappingsValidation.checkSalesOrganizationById(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDivisionNull() {
        when(divisionRepository.getDivisionIDandSalesIDById(
                any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        complaintReasonMappingsValidation.checkDivisionExistAndIsMappedWithSalesID(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryNull() {
        when(itemCategoriesDao.getComplaintItemCategory(
                any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        complaintReasonMappingsValidation.checkItemCategoryExist(null);
    }

    @Test
    public void beforeCreateItemCategoryBlank() {
        when(itemCategoriesDao.getComplaintItemCategory(
                any(String.class))).thenReturn(result);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(0L);
        when(result.list()).thenReturn(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        complaintReasonMappingsValidation.checkItemCategoryExist("");
    }

    @Test
    public void validateComplaintReasonMapExist() {
        MockitoAnnotations.openMocks(this);
        complaintReasonMappings = Struct.create(ComplaintReasonMappings.class);
        complaintReasonMappings.setSalesOrganizationId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDistributionChannelId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDivisionId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setItemCategoryId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setComplaintReasonId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        when(itemCategoriesDao
                .getComplaintItemCategory("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(complaintReasonsDao
                .getComplaintReasonBasedOnID("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(divisionRepository
                .getDivisionIDandSalesIDById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(distributionChannelRepository
                .getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(distributionChannelRepository.getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(divisionRepository.getDivisionIDandSalesIDById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(salesOrganizationRepository
                .getSalesOrganizationById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(complaintReasonMappingsDao.getComplaintReasonMapBasedOnAttributeIDs(
                any(String.class), any(String.class), any(String.class), any(String.class),
                any(String.class))).thenReturn(result);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        complaintReasonMappingsValidation.validateComplaintReasonMapExist(complaintReasonMappings);
    }


    @Test
    public void validateComplaintReasonMapExistBlank() {
        MockitoAnnotations.openMocks(this);
        complaintReasonMappings = Struct.create(ComplaintReasonMappings.class);
        complaintReasonMappings.setSalesOrganizationId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDistributionChannelId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setDivisionId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setItemCategoryId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        complaintReasonMappings.setComplaintReasonId("");
        when(itemCategoriesDao
                .getComplaintItemCategory("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(complaintReasonsDao
                .getComplaintReasonBasedOnID(""))
                .thenReturn(result);
        when(divisionRepository
                .getDivisionIDandSalesIDById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(distributionChannelRepository
                .getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(distributionChannelRepository.getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(divisionRepository.getDivisionIDandSalesIDById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(salesOrganizationRepository
                .getSalesOrganizationById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(complaintReasonMappingsDao.getComplaintReasonMapBasedOnAttributeIDs(
                any(String.class), any(String.class), any(String.class), any(String.class),
                any(String.class))).thenReturn(result);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        complaintReasonMappingsValidation.validateComplaintReasonMapExist(complaintReasonMappings);
    }

    @Test
    public void beforeCreateItemCategoryDistributionExistNull() {
        when(distributionChannelRepository.getDistributionChannelById(
                any(String.class))).thenReturn(result);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result1.rowCount()).thenReturn(0L);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDistributionChannelExistAndIsMappedWithSalesID(
                null,
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDistributionNotExistForSalesID() {
        when(distributionChannelRepository.getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        List<DistributionChannels> distributionChannelsList = new ArrayList<>();
        distributionChannels.setId("ab12");
        distributionChannels.setSalesOrganizationIDId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        distributionChannelsList.add(distributionChannels);
        when(result.listOf(DistributionChannels.class)).thenReturn(distributionChannelsList);
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", null);
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDistributionChannelExistAndIsMappedWithSalesID(
                null,
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDistributionExistForSalesID() {
        when(distributionChannelRepository.getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        List<DistributionChannels> distributionChannelsList = new ArrayList<>();
        distributionChannels.setId("ab12");
        distributionChannels.setSalesOrganizationIDId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        distributionChannelsList.add(distributionChannels);
        when(result.listOf(DistributionChannels.class)).thenReturn(distributionChannelsList);
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDistributionChannelExistAndIsMappedWithSalesID(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDistributionExistBlankForSalesID() {
        when(distributionChannelRepository.getDistributionChannelById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        List<DistributionChannels> distributionChannelsList = new ArrayList<>();
        distributionChannels.setId("ab12");
        distributionChannels.setSalesOrganizationIDId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        distributionChannelsList.add(distributionChannels);
        when(result.listOf(DistributionChannels.class)).thenReturn(distributionChannelsList);
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDistributionChannelExistAndIsMappedWithSalesID(
                "",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDivisionExistNull() {
        when(divisionRepository.getDivisionIDandSalesIDById(
                any(String.class))).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", null);
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDivisionExistAndIsMappedWithSalesID(
                null,
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDivisionExistBlank() {
        when(divisionRepository.getDivisionIDandSalesIDById(
                any(String.class))).thenReturn(result);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDivisionExistAndIsMappedWithSalesID(
                "",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryDivisionExistForSalesID() {
        when(divisionRepository.getDivisionIDandSalesIDById("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        List<Divisions> divisionsList = new ArrayList<>();
        divisions.setId("ab12");
        divisions.setSalesOrganizationIDId("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        divisionsList.add(divisions);
        when(result.listOf(Divisions.class)).thenReturn(divisionsList);
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(message1.target(any(String.class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        complaintReasonMappingsValidation.checkDivisionExistAndIsMappedWithSalesID(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7",
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }

    @Test
    public void beforeCreateItemCategoryExistAndActiveFalse() {
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(itemCategoriesDao
                .getComplaintItemCategory("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(itemCategoryService.getActive(anyString())).thenReturn(false);
        complaintReasonMappingsValidation.checkItemCategoryExist(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }
    @Test
    public void beforeCreateItemCategoryExistAndActiveTrue() {
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(itemCategoriesDao
                .getComplaintItemCategory("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(itemCategoryService.getActive(anyString())).thenReturn(true);
        complaintReasonMappingsValidation.checkItemCategoryExist(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }
    @Test
    public void beforeCreateReasonCategoryExistAndActiveTrue() {
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        when(complaintReasonsDao
                .getComplaintReasonBasedOnID("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(complaintReasonsService.getActive(anyString())).thenReturn(true);
        complaintReasonMappingsValidation.checkComplaintReasonExist(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }
    @Test
    public void beforeCreateReasonCategoryExistAndActiveFalse() {
        List<Row> rowValues = new ArrayList<>();
        Row row = Struct.create(Row.class);
        row.put("ID", "201");
        row.put("salesOrganization_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("distributionChannel_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("division_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemCategory_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        row.put("complaintItemReason_ID", "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.rowCount()).thenReturn(1L);
        when(result.list()).thenReturn(rowValues);
        when(result.first()).thenReturn(opt);
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(message1);
        when(messages.error("abc")).thenReturn(message1);
        when(complaintReasonsDao
                .getComplaintReasonBasedOnID("5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7"))
                .thenReturn(result);
        when(complaintReasonsService.getActive(anyString())).thenReturn(false);
        complaintReasonMappingsValidation.checkComplaintReasonExist(
                "5f6c9e0e-b3e0-41a3-8935-ece2bfa66af7");
    }
}
