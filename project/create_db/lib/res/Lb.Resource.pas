unit Lb.Resource;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo;

type
  TResFrame = class(TFrame)
    Memo: TMemo;
  private
  public
    procedure SetParam(const AName: String; const AStrings: TStrings);
  end;

procedure SetResourceParams(const AName: String; const AStrings: TStrings);

implementation

{$R *.fmx}

var
  localRes: TResFrame = nil;

function GetRes: TResFrame;
begin
  if not Assigned(localRes) then
    localRes := TResFrame.Create(nil);
  Result := localRes;
end;

procedure SetFinalization;
begin
  if Assigned(localRes) then
    FreeAndNil(localRes);
  localRes := nil;
end;

procedure SetResourceParams(const AName: String; const AStrings: TStrings);
begin
  GetRes.SetParam(AName,AStrings);
end;

{ TResFrame }

procedure TResFrame.SetParam(const AName: String; const AStrings: TStrings);

  function GetIsBegin(const S: String): Boolean;
  begin
    Result := Pos('__begin__',AnsiLowerCase(S)) = 1;
  end;

  function GetIsEnd(const S: String): Boolean;
  begin
    Result := Pos('__end__',S) = 1;
  end;

  function GetNameParam(const S: String): String;
  var
    xIndBegin, xIndEnd: Integer;
  begin
    Result := '';
    if GetIsBegin(S) then
    begin
      xIndBegin := Pos('(',S) + 1;
      xIndEnd := Pos(')',S);
      Result := Copy(S,xIndBegin,xIndEnd - xIndBegin);
    end;
  end;

var
  xS: String;
  xName: String;
  xBody: Boolean;
begin
  xBody := False;
  if Assigned(AStrings) then
  begin
    AStrings.Clear;
    for xS in Self.Memo.Lines do
    begin
      if Pos('##',xS) > 0 then
        Continue;
      if xBody then
      begin
        xBody := not GetIsEnd(xS);
        if xBody then
          AStrings.Add(xS);
      end
      else
      begin
        xBody := GetIsBegin(xS);
        if xBody then
        begin
          xName := GetNameParam(xS);
          xBody := SameText(xName,AName);
        end;
      end;
    end;
  end;
end;

initialization

finalization
  SetFinalization;


end.
