using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.plant.Plants with {

    ID
    @UI.Hidden : true
    @Common    : {
        Label : '{i18n>ID}',
        Text  : {
            $value                 : plant,
            ![@UI.TextArrangement] : #TextOnly
        }
    };
    plant
    @Common.Label: '{i18n>PLANT}';
   
    plantName
    @Common.Label: '{i18n>NAME}';

    plantNameExtension
    @UI.HiddenFilter;

    companyCodeID
    @UI.HiddenFilter;

    customerNoOfPlant
    @UI.HiddenFilter;

    supplierNoOfPlant
    @UI.HiddenFilter;

    addressID
    @UI.HiddenFilter;

    factoryCalendar
    @UI.HiddenFilter;
  
}

annotate cmh.plant.Plants with @(Capabilities.SearchRestrictions.Searchable : true);

annotate cmh.plant.Plant with @(Common.ValueListMapping : {
    Label          : '{i18n>PLANT}',
    CollectionPath : 'Plants',
    Parameters     : [
    {
        $Type             : 'Common.ValueListParameterInOut',
        ValueListProperty : 'ID',
        LocalDataProperty : plant_ID
    },
    {
        $Type             : 'Common.ValueListParameterDisplayOnly',
        ValueListProperty : 'plant',
        ![@UI.Importance] : #High
    },
    {
        $Type             : 'Common.ValueListParameterDisplayOnly',
        ValueListProperty : 'plantName',
        ![@UI.Importance] : #High
    },
    {
        $Type             : 'Common.ValueListParameterDisplayOnly',
        ValueListProperty : 'addressID/city',
        ![@UI.Importance] : #High
    },
    {
        $Type             : 'Common.ValueListParameterDisplayOnly',
        ValueListProperty : 'addressID/postalCode',
        ![@UI.Importance] : #High
    }
    ]
});
