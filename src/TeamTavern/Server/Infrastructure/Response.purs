module TeamTavern.Server.Infrastructure.Response where

import Prelude

import TeamTavern.Server.Infrastructure.Error (TerrorVar)
import Jarilo (BadRequestRow, ForbiddenRow, InternalRow, NotAuthorizedRow, NotFoundRow)



type BadRequestTerror body errors = TerrorVar (BadRequestRow body errors)

type NotFoundTerror body errors = TerrorVar (NotFoundRow body errors)

type NotAuthorizedTerror body errors = TerrorVar (NotAuthorizedRow body errors)

type ForbiddenTerror body errors = TerrorVar (ForbiddenRow body errors)

type InternalTerror body errors = TerrorVar (InternalRow body errors)



type BadRequestTerror_ errors = BadRequestTerror Unit errors

type NotFoundTerror_ errors = NotFoundTerror Unit errors

type NotAuthorizedTerror_ errors = NotAuthorizedTerror Unit errors

type ForbiddenTerror_ errors = ForbiddenTerror Unit errors

type InternalTerror_ errors = InternalTerror Unit errors
