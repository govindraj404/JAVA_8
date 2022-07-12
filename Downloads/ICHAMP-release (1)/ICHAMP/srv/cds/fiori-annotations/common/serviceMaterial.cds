using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.serviceMaterial.ServiceMaterials with {

	ID
	@UI.Hidden : true
	@Common    : {
		Label : '{i18n>ID}',
		Text  : {
			$value                 : serviceMaterial,
			![@UI.TextArrangement] : #TextOnly
		}
	};      
}

annotate cmh.serviceMaterial.ServiceMaterial with
@(Common.ValueListMapping : {

		Label          : '{i18n>SERVICE_MATERIAL}',
		CollectionPath : 'ServiceMaterials',
		Parameters     : [
		{
				$Type             : 'Common.ValueListParameterInOut',
				ValueListProperty : 'ID',
				LocalDataProperty : serviceMaterial_ID
		},
		{
				$Type             : 'Common.ValueListParameterDisplayOnly',
				ValueListProperty : 'serviceMaterial',
				![@UI.Importance] : #High
		}
		]
});