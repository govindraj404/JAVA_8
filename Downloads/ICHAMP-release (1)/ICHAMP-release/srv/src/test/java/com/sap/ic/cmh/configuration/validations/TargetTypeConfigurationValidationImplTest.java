package com.sap.ic.cmh.configuration.validations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import cds.gen.configurationservice.ItemCategories;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.service.TargetTypeConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.SecurityValidator;
import io.micrometer.core.instrument.util.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.TargetDocumentCategoryDao;
import com.sap.ic.cmh.configuration.service.TargetTypeConfigurationService;
import com.sap.ic.cmh.configuration.persistency.TargetTypeConfigurationDao;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import com.sap.ic.cmh.utils.SecurityValidator;
import com.sap.cds.Row;
import java.util.Optional;
import cds.gen.configurationservice.TargetTypes;
import cds.gen.configurationservice.TargetTypes_;

public class TargetTypeConfigurationValidationImplTest {

    @InjectMocks
    TargetTypeConfigurationValidationImpl targetTypeConfigurationValidationImpl;

    @Mock
    Messages messages;

    @Mock
    private Message msg;
    @Mock
    Message message1;
    @Mock
    DataValidator dataValidator;

    @Mock
    TargetTypeConfigurationDao targetTypeDao;

    @Mock
    TargetTypeConfigurationService targetTypeService;
    @Mock
    TargetDocumentCategoryDao targetDocumentCategoryDao;

    @Mock
    Result result;
    @Mock
    SecurityValidator securityValidator;
    @Mock
    protected PersistenceService mockDb;
    TargetTypes targetTypes;
    TargetTypes targetTypes2;
    private Row row;
    private Optional<Row> opt;

    @Before
    public void setBefore() {
        MockitoAnnotations.openMocks(this);
        targetTypes = Struct.create(TargetTypes.class);
        targetTypes2 = Struct.create(TargetTypes.class);
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("ID", "ID");
        row.put("identifier", "11");
        opt = Optional.of(row);
    }
    @Test
    public void testValidateDescription() {
        targetTypes.setDescription("desc");
        Mockito.when(securityValidator.isValidText(targetTypes.getDescription())).thenReturn(true);
        // when(securityValidator.isValidText("code")).thenReturn(Boolean.TRUE);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.INVALID_TARGET_TYPE_DESCRIPTION)).thenReturn(msg);
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(targetTypeService.getTargetTypeConfigurationIdBasedOnCode(any(String.class))).thenReturn(result);
        row.put("ID", "101");
        opt = Optional.of(row);
        when(result.first()).thenReturn(opt);
        List<Row> rows = new ArrayList<>();
        rows.add(row);
        when(result.list()).thenReturn(rows);
        targetTypes.setId("101");
        targetTypes.setCode("code");
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }

    @Test
    public void testValidateDescription1() {
        targetTypes.setDescription("desc");
        Mockito.when(securityValidator.isValidText(targetTypes.getDescription())).thenReturn(true);
        // when(securityValidator.isValidText("code")).thenReturn(Boolean.TRUE);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.INVALID_TARGET_TYPE_DESCRIPTION)).thenReturn(msg);
        when(messages.error("any", "test")).thenReturn(msg);
        when(msg.target("any")).thenReturn(msg);
        when(targetTypeService.getTargetTypeConfigurationIdBasedOnCode(any(String.class))).thenReturn(result);
        row.put("ID", "101");
        opt = Optional.of(row);
        when(result.first()).thenReturn(opt);
        List<Row> rows = new ArrayList<>();
        rows.add(row);
        when(result.list()).thenReturn(rows);
        targetTypes.setId("100");
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }
    @Test
    public void validateDescriptionForNull() {
        targetTypes.setDescription(null);
        Mockito.when(securityValidator.isValidText(targetTypes.getDescription())).thenReturn(true);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.INVALID_TARGET_TYPE_DESCRIPTION)).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }

    @Test
    public void testValidateDescriptionFalseNull() {
        targetTypes.setDescription(null);
        Mockito.when(securityValidator.isValidText(targetTypes.getDescription())).thenReturn(false);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.INVALID_TARGET_TYPE_DESCRIPTION)).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }
    @Test
    public void testValidateDescriptionFalse() {
        targetTypes.setDescription("desc");
        Mockito.when(securityValidator.isValidText(targetTypes.getDescription())).thenReturn(false);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        when(messages.error(MessageKeys.INVALID_TARGET_TYPE_DESCRIPTION)).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }
    @Test
    public void testValidateCodeIf() {
        targetTypes.setCode(null);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }

    @Test
    public void testValidateCodeElseIf() {
        targetTypes.setCode("code");
        targetTypes.setId("ID1");
        when(targetTypeService.getTargetTypeConfigurationIdBasedOnCode("code")).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }
    @Test
    public void testValidateCodeElseIf1() {
        opt=Optional.empty();
        targetTypes.setCode("code");
        when(targetTypeService.getTargetTypeConfigurationIdBasedOnCode("code")).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes);
    }
    @Test
    public void testValidateTargetDocumentaryCodeIfNull() {
        TargetTypes targetTypes1=Struct.create(TargetTypes.class);
        targetTypes1.setTargetDocumentCategoryCode(null);
        // when(targetDocumentCategoryDao.getTargetDocumentCategoryBasedOnCode("code")).thenReturn(result);
        // when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes1);
    }
    @Test
    public void testValidateTargetDocumentaryCodeElseIf() {
        TargetTypes targetTypes1=Struct.create(TargetTypes.class);
        targetTypes1.setTargetDocumentCategoryCode("code");

        when(targetDocumentCategoryDao.getTargetDocumentCategoryBasedOnCode(any(String.class))).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes1);
    }

    @Test
    public void testValidateTargetDocumentaryCodeElseIf1() {

        TargetTypes targetTypes1=Struct.create(TargetTypes.class);
        targetTypes1.setTargetDocumentCategoryCode("code");
        row = Struct.create(Row.class);
        row.put("code", "CODE100");
        row.put("ID", "ID");
        row.put("identifier", "11");
        opt = Optional.of(row);
        when(targetDocumentCategoryDao.getTargetDocumentCategoryBasedOnCode("code")).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes1);
    }
    @Test
    public void testValidateTargetDocumentaryCodeWithNullData() {
        opt=Optional.empty();
        TargetTypes targetTypes1=Struct.create(TargetTypes.class);
        targetTypes1.setTargetDocumentCategoryCode("code");
        when(targetDocumentCategoryDao.getTargetDocumentCategoryBasedOnCode("code")).thenReturn(result);
        when(result.first()).thenReturn(opt);
        when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
        when(msg.target(any(String.class))).thenReturn(msg);
        targetTypeConfigurationValidationImpl.validateTargetType(targetTypes1);
    }


}


