package com.sap.ic.cmh.configuration.validations;

import cds.gen.configurationservice.TargetReferenceTypeMappings;
import cds.gen.configurationservice.TargetReferenceTypes;
import cds.gen.configurationservice.TargetTypes;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.service.TargetTypeConfigurationService;
import com.sap.ic.cmh.gen.MessageKeys;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

public class TargetReferenceTypesValidationImplTest {

    @InjectMocks
    TargetReferenceTypesValidationImpl impl;
    @Mock
    Messages messages;

    @Mock
    Message message;
    @Mock
    TargetReferenceTypeMappings targetReferenceTypeMappings;
    @Mock
    TargetTypeConfigurationService targetTypeConfigurationService;
    @Before
    public void beforeClass()
    {
        MockitoAnnotations.openMocks(this);
        //targetReferenceTypeMappings= Struct.create(TargetReferenceTypeMappings.class);
    }

    @Test
    public void validateTargetReferenceTypesTest()
    {
        List<TargetReferenceTypes> targetReferenceTypes =new ArrayList<>();
        TargetReferenceTypes targetReference=Struct.create(TargetReferenceTypes.class);
        targetReference.setTargetTypeId(null);
        targetReference.setDestinationSystem("102");
        targetReferenceTypes.add(targetReference);
        targetReferenceTypeMappings.setTargetTypes(targetReferenceTypes);
        when(targetReferenceTypeMappings.getTargetTypes()).thenReturn(targetReferenceTypes);

        when(messages.error(any(String.class))).thenReturn(message);
        impl.validateTargetReferenceTypes(targetReferenceTypeMappings);
    }

    @Test
    public void validateTargetReferenceTypesInvalidTest()
    {
        List<TargetReferenceTypes> targetReferenceTypes =new ArrayList<>();
        TargetReferenceTypes targetReference=Struct.create(TargetReferenceTypes.class);
        targetReference.setTargetTypeId("102");
        targetReferenceTypes.add(targetReference);
        targetReferenceTypes.add(targetReference);
        targetReferenceTypeMappings.setTargetTypes(targetReferenceTypes);
        when(targetReferenceTypeMappings.getTargetTypes()).thenReturn(targetReferenceTypes);

        when(messages.error(any(String.class))).thenReturn(message);
        impl.validateTargetReferenceTypes(targetReferenceTypeMappings);
    }
    @Test
    public void validateTargetReferenceTypesBlankTest()
    {
        List<TargetReferenceTypes> targetReferenceTypes =new ArrayList<>();
        TargetReferenceTypes targetReference=Struct.create(TargetReferenceTypes.class);
        targetReference.setTargetTypeId("");
        targetReference.setDestinationSystem("102");

        targetReferenceTypes.add(targetReference);
        targetReferenceTypeMappings.setTargetTypes(targetReferenceTypes);
        when(targetReferenceTypeMappings.getTargetTypes()).thenReturn(targetReferenceTypes);
        when(targetTypeConfigurationService.getActive(anyString())).thenReturn(true);
        when(messages.error(any(String.class))).thenReturn(message);
        impl.validateTargetReferenceTypes(targetReferenceTypeMappings);
    }
    @Test
    public void validateTargetReferenceTypesNotNullDestinationTest()
    {
        TargetReferenceTypes targetReference=Struct.create(TargetReferenceTypes.class);
        targetReference.setDestinationSystem("");
        when(messages.error(MessageKeys.TARGET_TYPE_IS_MANDATORY)).thenReturn(message);
        when(messages.error(MessageKeys.DESTINATION_SYSTEM_IS_MANDATORY)).thenReturn(message);
        impl.validateTargetReferenceTypesNotNull(targetReference);
    }
}
