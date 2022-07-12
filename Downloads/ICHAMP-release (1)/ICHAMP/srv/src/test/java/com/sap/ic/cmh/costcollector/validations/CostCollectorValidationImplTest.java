package com.sap.ic.cmh.costcollector.validations;

import cds.gen.costcollectorservice.CostCollectors;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.validations.ConfigurationFieldsValidation;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.costcollector.persistance.CostCollectorDao;
import com.sap.ic.cmh.gen.MessageKeys;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class CostCollectorValidationImplTest {

    @InjectMocks
    @Autowired
    CostCollectorValidationImpl validator;

    @Mock
    CostCollectorDao costCollectorDao;

    @Mock
    Messages messages;

    @Mock
    Message message;


    @Mock
    ConfigurationFieldsValidation configurationFieldsValidation;

    @Mock
    MasterDataValidation masterDataValidation;

    private CostCollectors costCollector;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        costCollector = Struct.create(CostCollectors.class);
        costCollector.setClaim("CID123");
        costCollector.setDescription("faulty prod");
        costCollector.setId("C101");
        costCollector.setCurrencyCode("EUR");
        costCollector.setTotalCost(BigDecimal.TEN);
    }

    @Test
    public void testValidateCostCollectorFRAttributes() {
        when(messages.error(MessageKeys.SUB_ITEM_TYPE_IS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.NUMBER_IS_NEGATIVE)).thenReturn(message);
        validator.validateCostCollectorFRAttributes(costCollector);
    }

    @Test
    public void testValidateCostCollectorFRAttributesUnitCode() {
        costCollector.setUnitCode("FDER");
        when(messages.error(MessageKeys.SUB_ITEM_TYPE_IS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.NUMBER_IS_NEGATIVE)).thenReturn(message);
        validator.validateCostCollectorFRAttributes(costCollector);
    }

    @Test
    public void testValidateCostCollectorSUBLAttributes() {
        when(messages.error(MessageKeys.SUB_ITEM_TYPE_IS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.NUMBER_IS_NEGATIVE)).thenReturn(message);
        validator.validateCostCollectorSUBLAttributes(costCollector);
    }

    @Test
    public void testValidateCostCollectorFRAttributesNS() {
        when(configurationFieldsValidation.validateSubItemType(any())).thenReturn(true);
        when(messages.error(MessageKeys.NUMBER_IS_NEGATIVE)).thenReturn(message);
        validator.validateCostCollectorFRAttributes(costCollector);
    }

    @Test
    public void testValidateCostCollectorSUBLAttributesNS() {
        when(configurationFieldsValidation.validateSubItemType(any())).thenReturn(true);
        when(messages.error(MessageKeys.NUMBER_IS_NEGATIVE)).thenReturn(message);
        validator.validateCostCollectorSUBLAttributes(costCollector);
    }

}
