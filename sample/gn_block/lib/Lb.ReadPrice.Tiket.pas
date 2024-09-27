unit Lb.ReadPrice.Tiket;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TTiket = record
    Date : TDateTime;
    Time : TDateTime;
    Last : Double;
    Vol  : Double;
  public
    function ToStr: String;
  end;

  TTiketList = TList<TTiket>;

  TTiketsSource = class(TStringList)
  private
    function GetTikets(Index: Integer): TTiket;
  public
    property Tikets[Index: Integer]: TTiket read GetTikets;
  end;

implementation

{
<DATE>;<TIME>;<LAST>;<VOL>
240920;095934;122.3;10
240920;095934;122.3;30
}

function GetTiketToStr(const S: String): TTiket;

  function _StrToDate(S: String): TDateTime;
  var
    xS: String;
  begin
    xS := Copy(S,5,2) + '.' + Copy(S,3,2) + '.20' + Copy(S,1,2);
    Result := StrToDate(xS);
  end;

  function _StrToTime(S: String): TDateTime;
  var
    xS: String;
  begin
    xS := Copy(S,1,2) + ':' + Copy(S,3,2) + ':' + Copy(S,5,2);
    Result := StrToTime(xS);
  end;

  procedure _Paser(const S: String; ASource: TStrings);
  begin
    ASource.Clear;
    ASource.Delimiter := ';';
    ASource.DelimitedText := S;
  end;

var
  xTiket: TTiket;
  xSource: TStrings;
  xF: TFormatSettings;
begin
  xSource := TStringList.Create;
  try
    _Paser(S,xSource);

    xF := FormatSettings;
    xF.DecimalSeparator := '.';

    xTiket.Date  := _StrToDate(xSource[0]);
    xTiket.Time  := _StrToTime(xSource[1]);
    xTiket.Last  := StrToFloatDef(xSource[2],0, xF);
    xTiket.Vol   := StrToFloatDef(xSource[3],0, xF);

    Result := xTiket;
  finally
    FreeAndNil(xSource);
  end;
end;

{ TTiketsSource }

function TTiketsSource.GetTikets(Index: Integer): TTiket;
begin
  var xS := Self.Strings[Index];
  Result := GetTiketToStr(xS);
end;

{ TTiket }

function TTiket.ToStr: String;
begin
  var xS := '';
  xS := xS + DateToStr(Date) + '; ';
  xS := xS + TimeToStr(Time) + '; ';
  xS := xS + FloatToStr(Last) + '; ';
  xS := xS + FloatToStr(Vol);
  Result := xS;
end;

end.
