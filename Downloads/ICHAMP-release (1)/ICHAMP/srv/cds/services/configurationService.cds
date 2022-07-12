using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';

service ConfigurationService {
    entity ConditionTypes                      as projection on cmh.conditionType.ConditionTypes actions {
        action CopyConditionTypes() returns ConditionTypes;
    };

    entity ServiceMaterials                    as projection on cmh.serviceMaterial.ServiceMaterials actions {
        action CopyServiceMaterials() returns ServiceMaterials;
    };

    entity DestinationConfigurations           as projection on cmh.destinationConfiguration.DestinationConfigurations actions {
        action CopyDestinationConfigurations() returns DestinationConfigurations;
    };

    entity BusinessObjectConfigurations        as projection on cmh.businessObjectConfiguration.BusinessObjectConfigurations actions {
        action CopyBusinessObjectConfigurations() returns BusinessObjectConfigurations;
    };

    entity ClaimStatusMappings                 as projection on cmh.claimStatusMappingExtension.ClaimStatusMappingExtensions actions {
        action CopyClaimStatusMappings() returns ClaimStatusMappings;
    };

    @readonly
    entity Destinations {
        destination : String
    };

   entity TargetReferenceTypeMappings         as projection on cmh.targetReferenceTypeMapping.TargetReferenceTypeMappings
    {
     *,
      false as isInActive: Boolean
        }
        actions {
        action CopyTargetReferenceTypeMappings() returns TargetReferenceTypeMappings;
      @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
         )
        action DeactivateTargetReferenceTypeMappings() returns  TargetReferenceTypeMappings;
	    @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
        )
        action ReactivateTargetReferenceTypeMappings() returns TargetReferenceTypeMappings;
     };


    entity ComplaintChannels                   as projection on cmh.complaintChannel.ComplaintChannels {
        *,
              false as isInActive: Boolean
                }actions{
                action CopyComplaintChannels() returns ComplaintChannels;
                @(Common.SideEffects : {
                    TargetProperties : ['isActive','isInActive']
                    },
                    Common.IsActionCritical: true
                 )
                action DeactivateComplaintChannels() returns  ComplaintChannels;
        	    @(Common.SideEffects : {
                    TargetProperties : ['isActive','isInActive']
                    },
                    Common.IsActionCritical: true
                )
                action ReactivateComplaintChannels() returns ComplaintChannels;
    };

    entity ItemCategories                      as projection on cmh.itemCategory.ItemCategories {
        *,              
        false as isInActive: Boolean,
        1 as isFieldControlEnableMaterial : Integer,
        1 as isFieldControlEnableConditionType : Integer
    } actions {
        action CopyItemCategories() returns ItemCategories;
     @(Common.SideEffects: {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
         )
        action DeactivateItemCategory() returns  ItemCategories;
	    @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
        )
        action ReactivateItemCategory() returns ItemCategories;
    };

    entity ComplaintReasons                    as projection on cmh.complaintReason.ComplaintReasons {
     *,
      false as isInActive: Boolean
        } actions {
        action CopyComplaintReasons() returns ComplaintReasons;
         @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
         )
        action DeactivateComplaintReasons() returns  ComplaintReasons;
	    @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
        )
        action ReactivateComplaintReasons() returns ComplaintReasons;
    };


    entity ComplaintReasonMappings           as projection on cmh.complaintReasonMapping.ComplaintReasonMappings {
     *,
      false as isInActive: Boolean
        }actions{
        action CopyComplaintReasonMappings() returns ComplaintReasonMappings;
        @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
         )
        action DeactivateComplaintReasonMappings() returns  ComplaintReasonMappings;
	    @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
        )
        action ReactivateComplaintReasonMappings() returns ComplaintReasonMappings;
    };


    entity ComplaintTypeConfigurations         as projection on cmh.complaintTypeConfiguration.ComplaintTypeConfigurations {
        *,
        7 as isFieldControlIndividualComplaintType : Integer,
        false as isInActive: Boolean
    } actions {
         action CopyComplaintTypeConfigurations() returns ComplaintTypeConfigurations;
         @(Common.SideEffefts : {
                    TargetProperties : ['isActive','isInActive']
                    },
                    Common.IsActionCritical: true
          )
         action DeactivateComplaintTypeConfigurations() returns  ComplaintTypeConfigurations;
         @(Common.SideEffects : {
                    TargetProperties : ['isActive','isInActive']
                    },
                    Common.IsActionCritical: true
          )
         action ReactivateComplaintTypeConfigurations() returns ComplaintTypeConfigurations;
    };


    entity ReferenceTypes                      as projection on cmh.referenceType.ReferenceTypes {
     *,
      false as isInActive: Boolean
        }actions{
        action CopyReferenceTypes() returns ReferenceTypes;
        @(Common.SideEffets : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
         )
        action DeactivateReferenceTypes() returns  ReferenceTypes;
	    @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
        )
        action ReactivateReferenceTypes() returns ReferenceTypes;

    };

     entity TargetTypes                      as projection on cmh.targetType.TargetTypes {
         *,
          false as isInActive: Boolean
            }actions{
            action CopyTargetTypes() returns TargetTypes;
            @(Common.SideEffects : {
                TargetProperties : ['isActive','isInActive']
                },
                Common.IsActionCritical: true
             )
            action DeactivateTargetTypes() returns  TargetTypes;
    	    @(Common.SideEffects : {
                TargetProperties : ['isActive','isInActive']
                },
                Common.IsActionCritical: true
            )
            action ReactivateTargetTypes() returns TargetTypes;
    };


  entity SourceReferenceTypeMappings         as projection on cmh.sourceReferenceTypeMapping.SourceReferenceTypeMappings {
             *,
             7 as isFieldControlReferenceType : Integer,
             false as isInActive              : Boolean
         } actions {
         action CopySourceReferenceTypeMappings() returns SourceReferenceTypeMappings;
         @(Common.SideEffefts : {
             TargetProperties : ['isActive','isInActive']
             },
             Common.IsActionCritical: true
          )
         action DeactivateSourceReferenceTypeMappings() returns  SourceReferenceTypeMappings;
 	    @(Common.SideEffects : {
             TargetProperties : ['isActive','isInActive']
             },
             Common.IsActionCritical: true
         )
          action ReactivateSourceReferenceTypeMappings() returns  SourceReferenceTypeMappings;

     };

    entity ComplaintTypeToItemCategoryMappings as projection on cmh.complaintTypeToItemCategoryMapping.ComplaintTypeToItemCategoryMappings{
     *,
      false as isInActive: Boolean
        }actions{
        action CopyComplaintTypeToItemCategoryMappings() returns ComplaintTypeToItemCategoryMappings;
        @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
         )
        action DeactivateComplaintTypeToItemCategoryMappings() returns  ComplaintTypeToItemCategoryMappings;
	    @(Common.SideEffects : {
            TargetProperties : ['isActive','isInActive']
            },
            Common.IsActionCritical: true
        )
        action ReactivateComplaintTypeToItemCategoryMappings() returns ComplaintTypeToItemCategoryMappings;

    };

    @readonly
    entity SalesAreas                          as projection on cmh.salesArea.SalesAreas;

    @readonly
    entity ComplaintCategories                 as projection on cmh.complaintCategory.ComplaintCategories;

    entity ConfigureItems {
        key code                   : String(20);
            name                   : String(200);
            associatedApplications : Association to many ComplaintCategories;
    }


};
