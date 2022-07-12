package com.sap.ic.cmh.masterdata.subitemtype.validation;

import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.masterdataservice.ItemTypes;
import cds.gen.masterdataservice.SubItemTypes;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.masterdata.common.persistency.MasterDataDao;
import com.sap.ic.cmh.utils.datavalidation.DataValidatorImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class SubItemTypeValidatorImplTest {

    @Mock
    private DataValidatorImpl dataValidator;
    @Mock
    private MasterDataDao dao;
    @Mock
    private Messages messages;
    @Mock
    Message msg;
    @InjectMocks
    private SubItemTypeValidatorImpl validator;
    @Mock
    private PersistenceService mockDb;
    @Mock
    Result result;

    private SubItemTypes groups;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        groups = Struct.create(SubItemTypes.class);
        groups.setCode("1");
        groups.setDescription("test");
        groups.setItemTypeCode("1");

        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);


    }
    @Test
    public void checkInputsSanitized_ValidationPass() {
        Object [] obj={"test"};
        when(dao.getItemTypeCode("1")).thenReturn(result);
        when(messages.error("test",obj)).thenReturn(msg);
        when(msg.target("test")).thenReturn(msg);
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationFailedForAlphaNumericDataTest() {
        groups.setItemTypeCode("<html><head></head><body></body></html>");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void checkInputsSanitized_ValidationateDataTest() {
        groups.setDescription("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }

    @Test
    public void testCheckInputsSanitized_ValidationateData() {
        groups.setCode("Henry-Strasse");
        validator.checkInputsSanitized(groups);
    }
    @Test
    public void validateItemTypeCodeTest() {
        when(dao.getItemTypeCode(any())).thenReturn(result);
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateItemTypeCode("201");
    }

    @Test
    public void validateItemTypeCodeNullTest() {
        ItemTypes itemTypes = Struct.create(ItemTypes.class);
        itemTypes.setCode("202");
        List<ItemTypes> itemTypesList=new ArrayList<>();
        itemTypesList.add(itemTypes);
        Optional<ItemTypes> emptyOpt = Optional.of(itemTypes);
        when(mockDb.run(any(CqnSelect.class))).thenReturn(result);
        when(result.first(ItemTypes.class)).thenReturn(emptyOpt);
        when(dao.getItemTypeCode(any())).thenReturn(result);
        List<Row> rowValues = new ArrayList<>();
        Row row=Struct.create(Row.class);
        row.put("ID", "201");
        row.put("businessObjectType", "202");
        Optional<Row> opt = Optional.of(row);
        rowValues.add(row);
        when(result.first()).thenReturn(Optional.of(row));
        when(result.iterator()).thenReturn(rowValues.iterator());
        when(result.first()).thenReturn(Optional.of(row));
        validator.validateItemTypeCode("201");
    }
    @Test
    public void validateSubItemTypeFieldsTest(){
        SubItemTypes subItemTypes = Struct.create(SubItemTypes.class);
        subItemTypes.setCode("202");
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        Mockito.when(msg.target(any(String.class))).thenReturn(msg);
        validator.validateSubItemTypeFields(subItemTypes);
    }

    @Test
    public void checkInputsSanitizedTest(){
        SubItemTypes subItemTypes = Struct.create(SubItemTypes.class);
        subItemTypes.setCode("202");
        subItemTypes.setItemTypeCode("202");
        subItemTypes.setDescription("test");
        validator.checkInputsSanitized(subItemTypes);
    }
}
