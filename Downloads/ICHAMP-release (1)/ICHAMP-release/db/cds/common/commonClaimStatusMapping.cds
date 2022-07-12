namespace com.sap.ic.cmh.commonClaimStatusMapping;

using {com.sap.ic.cmh.claimStatusMapping.ClaimStatusMappings} from '../claim/index';
using {com.sap.ic.cmh.claimStatusMappingExtension.ClaimStatusMappingExtensions} from '../claim/index';

type CommonClaimStatusMapping : Association to one CommonClaimStatusMappings;

view CommonClaimStatusMappings as
  select from ClaimStatusMappings{
     key code   as code,
     key 'SAP' as value : String,
     name   as name,
     status as status
  }
  union
  select from ClaimStatusMappingExtensions{
     key code   as code,
     key 'EXT' as value : String,
     name   as name,
     status as status
  };