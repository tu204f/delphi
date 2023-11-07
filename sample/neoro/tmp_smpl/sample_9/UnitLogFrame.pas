unit UnitLogFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects;

type
  TLogFrame = class(TFrame)
    MemoLog: TMemo;
    TextTitle: TText;
  private
    FBuffer: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Log(S: String);
  end;

implementation

{$R *.fmx}

{ TLogFrame }

constructor TLogFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TStringList.Create;
end;

destructor TLogFrame.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TLogFrame.Log(S: String);
var
  iCount: Integer;
  xS: String;
begin
  xS := FormatDateTime('hh:nn:ss.zzz',Time) + '|| ' + S;
  FBuffer.Insert(0,xS);
  // Удаляем лишении строки
  while FBuffer.Count > 50 do
  begin
    iCount := FBuffer.Count;
    FBuffer.Delete(iCount - 1);
  end;
  MemoLog.Lines.Assign(FBuffer);
end;

end.
