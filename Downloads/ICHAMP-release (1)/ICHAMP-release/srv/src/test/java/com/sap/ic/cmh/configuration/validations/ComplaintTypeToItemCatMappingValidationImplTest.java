package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.*;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ComplaintTypeToItemCatMappingDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.configuration.service.ComplaintTypeToItemCatMappingServiceImpl;
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

public class ComplaintTypeToItemCatMappingValidationImplTest {

    @InjectMocks
    ComplaintTypeToItemCatMappingValidationImpl validator;

    @Mock
    ConfigurationFieldsValidation fieldsValidation;

    @Mock
    ComplaintTypeToItemCategoryMappings_ complaintTypeToItemCategoryMappings_;

    @Mock
    MasterDataValidation masterDataValidation;
    @Mock
    PersistenceService db;

    @Mock
    Messages messages;

    @Mock
    ItemCategories itemCategories;

    @Mock
    ComplaintTypeConfigurations complaintTypeConfigurations;

    @Mock
    Result result;

    @Mock
    Result result1;
    @Mock
    ItemCategoriesDao itemCategoriesDao;
    @Mock
    ComplaintTypeToItemCatMappingDao complaintTypeToItemCatMappingDao;

    @Mock
    private Message msg;

    @Mock
    ComplaintTypeToItemCategoryMappings complaintType;

    @Mock
    ComplaintTypeToItemCatMappingServiceImpl complaintTypeToItemCatMappingService;

    private Row row;
    private Row row1;
    private Optional<Row> opt;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        complaintType = Struct.create(ComplaintTypeToItemCategoryMappings.class);
        row = Struct.create(Row.class);
        row1 = Struct.create(Row.class);
        complaintType.setComplaintTypeId("");
        complaintType.setItemCategoryId("1234");
        complaintType.setId("1245");
        complaintType.setDivisionId("127");
    }

    @Test
    public void validateComplaintTypeToItemCategoryMappingsTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        row1.put("complaintType_ID", "100");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ItemCategories> ll = new ArrayList<>();
        itemCategories.setIndividualComplaint(true);
        ll.add(itemCategories);
        when(result.listOf(ItemCategories.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaint()).thenReturn(false);
        when(complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatMappingBasedOnUniqueFields(complaintType)).thenReturn(result1);
        validator.validateComplaintTypeToItemCategoryMappings(complaintType);
    }

    @Test
    public void validateComplaintTypeToItemCategoryMappingsTest1() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        row1.put("complaintType_ID", "100");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ItemCategories> ll = new ArrayList<>();
        itemCategories.setIndividualComplaint(true);
        ll.add(itemCategories);
        when(result.listOf(ItemCategories.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaint()).thenReturn(null);
        when(complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatMappingBasedOnUniqueFields(complaintType)).thenReturn(result1);
        validator.validateComplaintTypeToItemCategoryMappings(complaintType);
    }

    @Test
    public void validateComplaintTypeTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ComplaintTypeConfigurations> ll = new ArrayList<>();
        complaintTypeConfigurations.setIndividualComplaintType(true);
        ll.add(complaintTypeConfigurations);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaintType()).thenReturn(true);
        validator.validateComplaintType("1234");
    }

    @Test
    public void validateComplaintTypeNonEmptyAndIndividualComplaintTypeFalseTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ComplaintTypeConfigurations> ll = new ArrayList<>();
        complaintTypeConfigurations.setIndividualComplaintType(true);
        ll.add(complaintTypeConfigurations);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaintType()).thenReturn(false);
        when(complaintTypeToItemCatMappingService.getIsComplaintTypeActive(anyString())).thenReturn(false);
        validator.validateComplaintType("1234");
    }

    @Test
    public void validateComplaintTypeIsActiveTrueTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ComplaintTypeConfigurations> ll = new ArrayList<>();
        complaintTypeConfigurations.setIndividualComplaintType(true);
        ll.add(complaintTypeConfigurations);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaintType()).thenReturn(true);
        when(complaintTypeToItemCatMappingService.getIsComplaintTypeActive(anyString())).thenReturn(true);
        validator.validateComplaintType("1234");
    }

    @Test
    public void validateComplaintTypeIsActiveFalseTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ComplaintTypeConfigurations> ll = new ArrayList<>();
        complaintTypeConfigurations.setIndividualComplaintType(true);
        ll.add(complaintTypeConfigurations);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaintType()).thenReturn(true);
        when(complaintTypeToItemCatMappingService.getIsComplaintTypeActive(anyString())).thenReturn(false);
        validator.validateComplaintType("1234");
    }

    @Test
    public void validateComplaintTypeNonEmptyAndIndividualComplaintTypeNullTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ComplaintTypeConfigurations> ll = new ArrayList<>();
        complaintTypeConfigurations.setIndividualComplaintType(true);
        ll.add(complaintTypeConfigurations);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaintType()).thenReturn(null);
        validator.validateComplaintType("1234");
    }

    @Test
    public void validateComplaintTypeEmptyAndIndividualComplaintTypeFalseTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(db.run(any(CqnSelect.class))).thenReturn(result);
        row1 = Struct.create(Row.class);
        opt = Optional.empty();
        when(result.first()).thenReturn(opt);
        List<ComplaintTypeConfigurations> ll = new ArrayList<>();
        complaintTypeConfigurations.setIndividualComplaintType(true);
        ll.add(complaintTypeConfigurations);
        when(result.listOf(ComplaintTypeConfigurations.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaintType()).thenReturn(false);
        validator.validateComplaintType("1234");
    }

    @Test
    public void validateIfRecordWithUniqueFieldsExistsTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        List<ItemCategories> ll = new ArrayList<>();
        itemCategories.setIndividualComplaint(true);
        ll.add(itemCategories);
        when(result.listOf(ItemCategories.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaint()).thenReturn(true);
        when(complaintTypeToItemCatMappingDao.getComplaintTypeToItemCatMappingBasedOnUniqueFields(complaintType)).thenReturn(result1);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result1.first()).thenReturn(opt);
        validator.validateIfRecordWithUniqueFieldsExists(complaintType);
    }

    @Test
    public void validateItemCategoryTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result1.first()).thenReturn(opt);
        validator.validateItemCategory("");
    }

    @Test
    public void validateItemCategoryEmptyTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        row1.put("complaintType_ID", "1234");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result1.first()).thenReturn(opt);
        validator.validateItemCategory("1234");
        row1 = Struct.create(Row.class);
        opt = Optional.empty();
        when(result.first()).thenReturn(opt);
        validator.validateItemCategory("1234");
    }

    @Test
    public void validateItemCategoryEmptyAndIndividualComplaintNotEmptyTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        row1.put("complaintType_ID", "100");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ItemCategories> ll = new ArrayList<>();
        itemCategories.setIndividualComplaint(true);
        ll.add(itemCategories);
        when(result.listOf(ItemCategories.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaint()).thenReturn(true);
        validator.validateItemCategory("1234");
    }

    @Test
    public void validateItemCategoryEmptyAndItemCategoryActiveTrueTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        row1.put("complaintType_ID", "100");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ItemCategories> ll = new ArrayList<>();
        itemCategories.setIndividualComplaint(true);
        ll.add(itemCategories);
        when(result.listOf(ItemCategories.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaint()).thenReturn(true);
        when(complaintTypeToItemCatMappingService.getIsItemCategoryActive(anyString())).thenReturn(true);
        validator.validateItemCategory("1234");
    }

    @Test
    public void validateItemCategoryEmptyAndItemCategoryActiveFalseTest() {
        when(messages.error(anyString())).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        when(itemCategoriesDao.getComplaintItemCategory(anyString())).thenReturn(result);
        row1.put("complaintType_ID", "100");
        row1.put("itemCategory_ID", "100");
        opt = Optional.of(row1);
        when(result.first()).thenReturn(opt);
        List<ItemCategories> ll = new ArrayList<>();
        itemCategories.setIndividualComplaint(true);
        ll.add(itemCategories);
        when(result.listOf(ItemCategories.class)).thenReturn(ll);
        when(ll.get(0).getIndividualComplaint()).thenReturn(true);
        when(complaintTypeToItemCatMappingService.getIsItemCategoryActive(anyString())).thenReturn(false);
        validator.validateItemCategory("1234");
    }

}