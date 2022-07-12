using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.materialMasterGeneralData.MaterialMasterGeneralDatas with {

	ID
	@UI.Hidden : true
	@Common    : {
		Label : '{i18n>ID}',
		Text  : {
			$value                 : materialCode,
			![@UI.TextArrangement] : #TextOnly
		}
	};
	materialCode
	@Common.Label: '{i18n>MATERIAL_CODE}';

	materialType
    @UI.HiddenFilter;

	materialGroup
    @UI.HiddenFilter;

	division
    @UI.HiddenFilter;

	xPlantMaterialStatus
    @UI.HiddenFilter;

	validFrom
    @UI.HiddenFilter;

	grossWeight
    @UI.HiddenFilter;

	weightUnit
    @UI.HiddenFilter;

	netWeight
	@UI.HiddenFilter;

	volume
	@UI.HiddenFilter;

	volumeUnit
	@UI.HiddenFilter;

	sizeDimensions
	@UI.HiddenFilter;

	EAN_UPC
	@UI.HiddenFilter;
	
	EANCategary
	@UI.HiddenFilter;

	productCompositionIndicator
	@UI.HiddenFilter;

	packagingMaterialGroup
	@UI.HiddenFilter;
	
	materialDescription
	@Common.Label: '{i18n>MATERIAL_DESCRIPTION}';

	grossWeight
	@Measures.Unit : weightUnit_code;

	baseUnitOfMeasure_code
	@UI.Hidden
	@Common.Label: '{i18n>UNIT}';

	baseUnitOfMeasure
	@UI.Hidden
	@Common.Label: '{i18n>UNIT}';

	volume
    @Measures.Unit        : volumeUnit_code;
       
}

annotate cmh.materialMasterGeneralData.MaterialMasterGeneralDatas with @(Capabilities.SearchRestrictions.Searchable : true);

annotate cmh.materialMasterGeneralData.MaterialMasterGeneralData with
@(Common.ValueListMapping : {

		Label          : '{i18n>MATERIAL}',
		CollectionPath : 'MaterialMasterGeneralDatas',
		Parameters     : [
		{
				$Type             : 'Common.ValueListParameterInOut',
				ValueListProperty : 'ID',
				LocalDataProperty : material_ID
		},
		{
				$Type             : 'Common.ValueListParameterOut',
				ValueListProperty : 'baseUnitOfMeasure_code',
				LocalDataProperty : unit_code
		},
		{
				$Type             : 'Common.ValueListParameterDisplayOnly',
				ValueListProperty : 'materialCode',
				![@UI.Importance] : #High
		},
		{
				$Type             : 'Common.ValueListParameterDisplayOnly',
				ValueListProperty : 'materialDescription',
				![@UI.Importance] : #High
		}
		]
});

annotate cmh.materialMasterGeneralData.MaterialMasterGeneralDatas with @(
	UI.HeaderInfo : {
		TypeName       : '{i18n>MATERIAL}',
		TypeNamePlural : '{i18n>MATERIALS}',
	Title : 
	{
	$Type : 'UI.DataField',
	Value : materialDescription
	},
	Description : {
	$Type : 'UI.DataField',
	Value : materialCode
	},
	ImageUrl : 'sap-icon://product'
	},
	UI.QuickViewFacets:[
	{
	$Type : 'UI.ReferenceFacet',
	Label :'{i18n>MATERIAL_DETAILS}',
	Target :'@UI.FieldGroup#QuickView'
	}
	],
	UI.FieldGroup#QuickView : { 
	Data : [
	{
	$Type : 'UI.DataField',
	Value : sizeDimensions,
	Label :'{i18n>DIMENSON}'
	},
	{
	$Type : 'UI.DataField',
	Value : grossWeight,
	Label :'{i18n>WEIGHT}'
	},
	{
	$Type : 'UI.DataField',
	Value : volume,
	Label :'{i18n>VOLUME}'
	}
	]
	}
	);