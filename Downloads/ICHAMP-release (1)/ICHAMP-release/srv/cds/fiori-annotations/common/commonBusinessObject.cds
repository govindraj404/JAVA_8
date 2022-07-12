using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.combineBusinessObject.CommonBusinessObjects with {

    ID
    @UI.Hidden : true
    @Common    : {
        Label : '{i18n>BUSINESS_OBJECT_NUMBER}',
        Text  : {
            $value                 : identifier,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    identifier
    @Search.defaultSearchElement
    @Common    : {Label : '{i18n>BUSINESS_OBJECT_NUMBER}'};
    
    @Common.FieldControl  : #ReadOnly
    personResponsible;

    status
    @Common               : {
        Text            : status.name,
        TextArrangement : #TextOnly
    };

};

annotate cmh.combineBusinessObject.CommonBusinessObjects with @(Capabilities.SearchRestrictions.Searchable : true);

annotate cmh.combineBusinessObject.CommonBusinessObject with @(
    Common.ValueListMapping : {
        Label          : '{i18n>BUSINESS_PARTNER}',
        CollectionPath : 'CommonBusinessObjects',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : businessObjectID_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'identifier',
                ![@UI.Importance] : #High
            }
        ]
    }
);