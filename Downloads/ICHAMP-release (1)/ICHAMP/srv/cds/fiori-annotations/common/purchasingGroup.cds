using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.purchasingGroup.PurchasingGroups with {
    code
    @Common : {	
        Label                    : '{i18n>PURCHASING_GROUP}',	
        Text  : {	
            $value                : description,	
           ![@UI.TextArrangement]  : #TextOnly	
            }	
    };

    description @Common : {
        Label        : '{i18n>DESCRIPTION}'
    };
};

annotate cmh.purchasingGroup.PurchasingGroup with @(    
    Common.ValueListMapping : {
        Label          : '{i18n>PURCHASING_GROUP}',
        CollectionPath : 'PurchasingGroups',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : purchasingGroup_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description',
                ![@UI.Importance] : #High
            }
        ]
    }
);
