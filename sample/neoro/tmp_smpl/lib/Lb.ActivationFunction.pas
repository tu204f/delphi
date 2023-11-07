(******************************************************************************)
(* Функция активации                                                          *)
(******************************************************************************)
unit Lb.ActivationFunction;

interface

type
  TTypeFuction = (tfIdentical,tfSingleStep,tfSigma,tfReLU, tfTh);

  ///<summary>Уравнение</summary>
  TEquations = function(const AValue: Double): Double;

///<summary>Функцион</summary>
function GetEquations(ATypeFuction: TTypeFuction): TEquations;
///<summary>Производная (дифференциал)</summary>
function GetDeffEquations(ATypeFuction: TTypeFuction): TEquations;

implementation

(******************************************************************************)
(* Тождественная                                                              *)
(******************************************************************************)

function Identical(const AValue: Double): Double;
begin
  Result := AValue;
end;

function DifIdentical(const AValue: Double): Double;
begin
  Result := 1;
end;

(******************************************************************************)
(* Единичная ступенька   ступенькой переключается 0 до 1                      *)
(******************************************************************************)

function SingleStep(const AValue: Double): Double;
begin
  if AValue < 0 then
    Result := 0
  else
    Result := 1;
end;


function DifSingleStep(const AValue: Double): Double;
begin
  if AValue < 0 then
    Result := 0
  else if AValue = 0 then
    Result := 0.5
  else
    Result := 1;
end;

(******************************************************************************)
(* Сигмойд  от 0 до 1                                                         *)
(******************************************************************************)

function Sigma(const AValue: Double): Double;
begin
  Result := 1 / (1 + Exp((-1 * AValue)));
end;

function DifSigma(const AValue: Double): Double;
begin
  Result := AValue * (1 - AValue);
end;

(******************************************************************************)
(* ReLU - Линейный выпрямител, можно применть с учтом параметра   0, бесконечность
(******************************************************************************)

function ReLU(const AValue: Double): Double;
begin
  if AValue < 0 then
    Result := 0
  else
    Result := AValue;
end;

function DifReLU(const AValue: Double): Double;
begin
  if AValue < 0 then
    Result := 0
  else
    Result := 1;
end;

(******************************************************************************)
(* Гиперболический тангес
(******************************************************************************)

function Th(const AValue: Double): Double;
begin
  Result := (Exp(AValue) - Exp(-1 * AValue))/(Exp(AValue) + Exp(-1 * AValue));
end;

function DifTh(const AValue: Double): Double;
var
  xV: Double;
begin
  xV := Th(AValue);
  Result := 1 - xV * xV;
end;

(******************************************************************************)

function GetEquations(ATypeFuction: TTypeFuction): TEquations;
begin
  Result := Sigma;
  case ATypeFuction of
    tfIdentical: Result := Identical;
    tfSingleStep: Result := SingleStep;
    tfSigma: Result := Sigma;
    tfReLU: Result := ReLU;
    tfTh: Result := Th;
  end;
end;

function GetDeffEquations(ATypeFuction: TTypeFuction): TEquations;
begin
  Result := DifSigma;
  case ATypeFuction of
    tfIdentical: Result := DifIdentical;
    tfSingleStep: Result := DifSingleStep;
    tfSigma: Result := DifSigma;
    tfReLU: Result := DifReLU;
    tfTh: Result := DifTh;
  end;
end;



end.
