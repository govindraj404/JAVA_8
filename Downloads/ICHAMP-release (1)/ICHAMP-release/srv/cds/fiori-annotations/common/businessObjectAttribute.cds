using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.businessObjectAttribute.BusinessObjectAttributes with {
    businessObjectAttribute
    @Common : {
        Label : '{i18n>BUSINESS_OBJECT_ATTRIBUTES}',
        Text  : {
            $value                : name,
            ![@UI.TextArrangement]  : #TextOnly
        }
    };
    
    
    @UI.Hidden
    businessObjectType;
};

annotate cmh.businessObjectAttribute.BusinessObjectAttribute with @(
    Common.ValueListMapping : {
        Label          : '{i18n>BUSINESS_OBJECT_ATTRIBUTES}',
        CollectionPath : 'BusinessObjectAttributes',
        Parameters     : [
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'businessObjectAttribute',
            LocalDataProperty : businessObjectAttribute_code
        },
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'businessObjectType',
            LocalDataProperty : businessObjectType_code
        }
        ]
    },
    Common.ValueListWithFixedValues
);
