unit Lb.Core.Variants;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants;

type
  TVariantHelper = record helper for Variant
  private
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsDouble: Double;
    function GetAsBoolean: Boolean;
    function GetAsInt64: Int64;
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: String);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsInt64(const Value: Int64);
  public
    function IsNull: Boolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsString: String read GetAsString write SetAsString;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
  end;

implementation

{ TVariantHelper }

function TVariantHelper.IsNull: Boolean;
begin
  Result := VarIsNull(Self);
end;

function TVariantHelper.GetAsInteger: Integer;
begin
  if Self.IsNull then
    Result := 0
  else
    Result := Self;
end;

procedure TVariantHelper.SetAsInteger(const Value: Integer);
begin
  Self := Value;
end;

function TVariantHelper.GetAsString: String;
begin
  Result := VarToStrDef(Self,'');
end;

procedure TVariantHelper.SetAsString(const Value: String);
begin
  Self := Value;
end;

function TVariantHelper.GetAsDouble: Double;
begin
  if Self.IsNull then
    Result := 0
  else
    Result := Self;
end;

procedure TVariantHelper.SetAsDouble(const Value: Double);
begin
  Self := Value;
end;

function TVariantHelper.GetAsBoolean: Boolean;
begin
  if Self.IsNull then
    Result := False
  else
    Result := Self;
end;

procedure TVariantHelper.SetAsBoolean(const Value: Boolean);
begin
  Self := Value;
end;

function TVariantHelper.GetAsInt64: Int64;
begin
  if Self.IsNull then
    Result := 0
  else
    Result := Self;
end;

procedure TVariantHelper.SetAsInt64(const Value: Int64);
begin
  Self := Value;
end;

end.
