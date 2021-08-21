unit Lb.SysUtils.ISO860;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes;

function GetStrISO860ToDateTime(const S: String): TDateTime;
function GetStrISO860ToDate(const S: String): TDateTime;
function GetStrISO860ToTime(const S: String): TDateTime;
function GetDateTimeToStrISO860(const ADateTime: TDateTime): String;
function GetDateToStrISO860(const ADateTime: TDateTime): String;
function GetTimeToStrISO860(const ADateTime: TDateTime): String;
function GetIsISO(const S: String): Boolean;

implementation

type
  TRecDT = record
    Date: String;
    Time: String;
  private
    function GetDateTime: String;
  public
    function IsISO: Boolean;
    procedure SetToStr(const S: String);
    property DateTime: String read GetDateTime;
  end;

{ TRecDT }

function TRecDT.IsISO: Boolean;
var
  xS: String;
begin
  try
    xS := Self.GetDateTime;
    StrToDateTime(xS);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TRecDT.SetToStr(const S: String);

  function GetDateStr(const AStrings: TStrings): String;
  begin
    Result := '';
    if AStrings.Count >= 3 then
      Result :=  AStrings[2] + '.' + AStrings[1] + '.' + AStrings[0];
  end;

  function GetTimeStr(const AStrings: TStrings): String;
  var
    xInd: Integer;
  begin
    Result := '';
    xInd := 0;
    if AStrings.Count = 6 then
      xInd := 3;
    if AStrings.Count >= 3 then
      Result := AStrings[xInd] + ':' + AStrings[xInd + 1] + ':' + AStrings[xInd + 2];
  end;

var
  xC: Char;
  xTmpS: String;
  xStr: TStrings;
  xDateFlag, xTimeFlag: Boolean;
begin
  Self.Date := '';
  Self.Time := '';
  //     yyyy-mm-dd hh:mm:ss
  xTmpS := '';
  // xS := '2020-08-10 11:20:31';
  // xS := '2020-08-10';
  // xS := '11:20:31';
  xDateFlag := False;
  xTimeFlag := False;
  if not S.IsEmpty then
  begin
    xStr := TStringList.Create;
    try
      for xC in S do
      begin
        if CharInSet(xC,[':','-',' ']) then
        begin
          if not xDateFlag then
            xDateFlag := xC = '-';
          if not xTimeFlag then
            xTimeFlag := xC = ':';
          if not xTmpS.IsEmpty then
            xStr.Add(xTmpS);
          xTmpS := '';
        end
        else
          xTmpS := xTmpS + xC;
      end;
      if not xTmpS.IsEmpty then
        xStr.Add(xTmpS);
      if xDateFlag then
        Self.Date := GetDateStr(xStr);
      if xTimeFlag then
        Self.Time := GetTimeStr(xStr);
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

function TRecDT.GetDateTime: String;
begin
  Result := Self.Date + ' ' + Self.Time;
end;

(******************************************************************************)

function GetIsISO(const S: String): Boolean;
var
  xR: TRecDT;
begin
  xR.SetToStr(S);
  Result := xR.IsISO;
end;

function GetStrISO860ToDateTime(const S: String): TDateTime;
var
  xR: TRecDT;
begin
  xR.SetToStr(S);
  Result := StrToDateTimeDef(xR.DateTime,0);
end;

function GetStrISO860ToDate(const S: String): TDateTime;
var
  xR: TRecDT;
begin
  xR.SetToStr(S);
  Result := StrToDateDef(xR.Date,0);
end;

function GetStrISO860ToTime(const S: String): TDateTime;
var
  xR: TRecDT;
begin
  xR.SetToStr(S);
  Result := StrToTimeDef(xR.Time,0)
end;

(******************************************************************************)


function GetDateTimeToStrISO860(const ADateTime: TDateTime): String;
begin
  Result := '';
  if ADateTime > 0 then
    Result := FormatDateTime('yyyy-mm-dd hh:mm:ss',ADateTime);
end;

function GetDateToStrISO860(const ADateTime: TDateTime): String;
begin
  Result := '';
  if ADateTime > 0 then
    Result := FormatDateTime('yyyy-mm-dd',ADateTime);
end;

function GetTimeToStrISO860(const ADateTime: TDateTime): String;
begin
  Result := '';
  if ADateTime > 0 then
    Result := FormatDateTime('hh:mm:ss',ADateTime);
end;

end.
