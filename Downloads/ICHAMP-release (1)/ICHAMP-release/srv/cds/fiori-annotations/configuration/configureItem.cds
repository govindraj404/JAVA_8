using ConfigurationService from '../../services/index';

annotate ConfigurationService.ConfigureItems {

    code
    @Common : {
        Label            : '{i18n>CONFIGURE_ITEM}',
        Text             : {
            $value                 : name,
            ![@UI.TextArrangement] : #TextOnly
        },
        ValueListMapping : {
            Label          : '{i18n>CONFIGURE_ITEMS}',
            CollectionPath : 'ConfigureItems',
            Parameters     : [{
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : code
            }]
        },
        ValueListWithFixedValues
    };


    name
    @UI.HiddenFilter;

    associatedApplications
    @UI.HiddenFilter;
};


annotate ConfigurationService.ComplaintCategories with {
    code
    @Common : {
        Label            : '{i18n>COMPLAINT_CATEGORY}',
        ValueListMapping : {
            Label          : '{i18n>COMPLAINT_CATEGORY}',
            CollectionPath : 'ComplaintCategories',
            Parameters     : [
                {
                    $Type             : 'Common.ValueListParameterInOut',
                    ValueListProperty : 'code',
                    LocalDataProperty : code
                },
                {
                    $Type             : 'Common.ValueListParameterDisplayOnly',
                    ValueListProperty : 'name'
                }
            ]
        },
        ValueListWithFixedValues
    };
};
