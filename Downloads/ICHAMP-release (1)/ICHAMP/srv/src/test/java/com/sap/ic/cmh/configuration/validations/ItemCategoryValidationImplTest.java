package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.persistency.ItemCategoriesDao;
import com.sap.ic.cmh.customercomplaint.complaintquantityrules.persistency.ComplaintQuantityRulesDao;
import com.sap.ic.cmh.masterdata.materialmastergeneraldata.repository.MaterialMasterGeneralDataRepository;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
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

public class ItemCategoryValidationImplTest {

    @InjectMocks
    ItemCategoryValidationImpl validator;

    @Mock
    DataValidator dataValidator;

    @Mock
    Messages messages;

    @Mock
    private Message msg;

    @Mock
    ItemCategoriesDao itemCategoryDao;

    @Mock
    ComplaintsDao complaintsDao;

    @Mock
    ComplaintQuantityRulesDao complaintQuantityRulesDao;

    @Mock
    Result result;

    @Mock
    MaterialMasterGeneralDataRepository materialMasterGeneralDataRepository;

    ItemCategories itemCategories;
    ItemCategories itemCategory;
    private Row row;
    private Optional<Row> opt;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        itemCategories = Struct.create(ItemCategories.class);
        itemCategory = Struct.create(ItemCategories.class);

        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("ID", "ID");
        row.put("identifier", "11");
        opt = Optional.of(row);
    }

    @Test
    public void testValidateItemCategories() {
        itemCategory.setMaterialEnteredManually(false);
        itemCategory.setReceivedQuantityEditable(false);
        itemCategory.setReturnQuantityEditable(false);
        itemCategory.setIndividualComplaint(false);
        itemCategory.setExternalReferenceMandatory(false);
        itemCategory.setExternalReferenceCheckedForDuplication(false);

        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateItemCategories(itemCategory);
    }

    @Test
    public void testValidateCode() {
        itemCategories.setCode("code");
        itemCategories.setId("ID1");
        itemCategory.setCode("code");
        itemCategory.setId("ID");
        List<ItemCategories> list = new ArrayList<>();
        List<Row> rowList = new ArrayList<>();
        rowList.add(row);
        list.add(itemCategory);
        when(itemCategoryDao.getItemCategoryBasedOnCode("code")).thenReturn(result);
        when(result.list()).thenReturn(rowList);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateCode(itemCategories);
    }
    @Test
    public void testValidateDescription() {
        itemCategory.setDescription("desc");
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateDescription(itemCategory);
    }

    @Test
    public void testValidateMaterialWithEmptyList() {
        itemCategory.setMaterialId("id");
        opt = Optional.empty();
        when(materialMasterGeneralDataRepository.getMaterialMasterGeneralDataBasedOnId(any(String.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMaterial(itemCategory);
    }

    @Test
    public void testValidateMaterial() {
        itemCategory.setMaterialId("id");
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        when(materialMasterGeneralDataRepository.getMaterialMasterGeneralDataBasedOnId(any(String.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateMaterial(itemCategory);
    }


    @Test
    public void testValidateComplaintCategoryWithEmptyData() {
        opt = Optional.empty();
        itemCategory.setComplaintCategoryCode("code");
        when(complaintsDao.getComplaintCategoryBasedOnCode("code")).thenReturn(result);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(result.first()).thenReturn(opt);
        validator.validateComplaintCategory(itemCategory);
    }

    @Test
    public void testValidateComplaintCategory() {
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        itemCategory.setComplaintCategoryCode("code");
        when(complaintsDao.getComplaintCategoryBasedOnCode("code")).thenReturn(result);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(result.first()).thenReturn(opt);
        validator.validateComplaintCategory(itemCategory);
    }

    @Test
    public void testValidateComplaintQuantityRuleWithEmptyData() {
        opt = Optional.empty();
        itemCategory.setComplaintQuantityRuleCode("code");
        when(complaintQuantityRulesDao.getComplaintQuantityRulesBasedOnCode("code")).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateComplaintQuantityRule(itemCategory);
    }

    @Test
    public void testValidateComplaintQuantityRule() {
        Row row = Struct.create(Row.class);
        Optional<Row> opt = Optional.of(row);
        itemCategory.setComplaintQuantityRuleCode("code");
        when(complaintQuantityRulesDao.getComplaintQuantityRulesBasedOnCode("code")).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateComplaintQuantityRule(itemCategory);
    }

}
