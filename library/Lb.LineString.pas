(******************************************************************************)
(* ������������� - ��� ����� ������ � ���� ������                             *)
(******************************************************************************)
unit Lb.LineString;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;

///<summary>������������� - ��� ����� ������ � ���� ������</summary>
function GetLineSource(const AValue, ASource: String; ASpace: String = ' '): String;
function GetLineParam(const AName, AValue, ASource: String; ASpace: String = ' '): String;

implementation

function GetLineSource(const AValue, ASource: String; ASpace: String = ' '): String;
begin
  // ������������� - ��� ����� ������ � ���� ������
  Result := ASource + AValue + ASpace;
end;

function GetLineParam(const AName, AValue, ASource: String; ASpace: String = ' '): String;
var
  xS: String;
begin
  xS := AName + ': ' + AValue;
  Result := GetLineSource(xS,ASource,ASpace);
end;

end.
