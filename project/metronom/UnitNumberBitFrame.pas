unit UnitNumberBitFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.Objects;

type
  TNumberBitFrame = class(TFrame)
    Layout: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelBit: TGridPanelLayout;
    Layout1: TLayout;
    Circle1: TCircle;
    Layout2: TLayout;
    Circle2: TCircle;
    Layout3: TLayout;
    Circle3: TCircle;
    Layout4: TLayout;
    Circle4: TCircle;
    Layout5: TLayout;
    Circle5: TCircle;
    Layout6: TLayout;
    Circle6: TCircle;
    Layout7: TLayout;
    Circle7: TCircle;
    Layout8: TLayout;
    Circle8: TCircle;
    Layout9: TLayout;
    RoundRect1: TRoundRect;
    Layout10: TLayout;
    RoundRect2: TRoundRect;
    Layout11: TLayout;
    Layout12: TLayout;
    Layout13: TLayout;
    Layout14: TLayout;
    Layout15: TLayout;
    Layout16: TLayout;
    LayoutNumberBit: TLayout;
    Text: TText;
  private
    FNumberBeat: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBeat;
    procedure SetStart;
    property NumberBeat: Integer read FNumberBeat;
  end;

implementation

{$R *.fmx}

{ TNumberBitFrame }

constructor TNumberBitFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNumberBeat := 0;
end;

destructor TNumberBitFrame.Destroy;
begin

  inherited;
end;

procedure TNumberBitFrame.SetBeat;

  procedure _Clear;
  begin
    Circle1.Fill.Color := $FFE0E0E0;
    Circle2.Fill.Color := $FFE0E0E0;
    Circle3.Fill.Color := $FFE0E0E0;
    Circle4.Fill.Color := $FFE0E0E0;
    Circle5.Fill.Color := $FFE0E0E0;
    Circle6.Fill.Color := $FFE0E0E0;
    Circle7.Fill.Color := $FFE0E0E0;
    Circle8.Fill.Color := $FFE0E0E0;
    RoundRect1.Fill.Color := $FFE0E0E0;
    RoundRect2.Fill.Color := $FFE0E0E0;
  end;

begin
  _Clear;
  case FNumberBeat of
    1: begin
      Circle1.Fill.Color := TAlphaColorRec.Lime;
      RoundRect1.Fill.Color := TAlphaColorRec.Lime
    end;
    2: begin
      Circle2.Fill.Color := TAlphaColorRec.Lime;
    end;
    3: begin
      Circle3.Fill.Color := TAlphaColorRec.Lime;
    end;
    4: begin
      Circle4.Fill.Color := TAlphaColorRec.Lime;
    end;
    5: begin
      Circle5.Fill.Color := TAlphaColorRec.Lime;
      RoundRect2.Fill.Color := TAlphaColorRec.Lime
    end;
    6: begin
      Circle6.Fill.Color := TAlphaColorRec.Lime;
    end;
    7: begin
      Circle7.Fill.Color := TAlphaColorRec.Lime;
    end;
    8: begin
      Circle8.Fill.Color := TAlphaColorRec.Lime;
    end;
  else
    raise Exception.Create('Error Message: Очень грубая ошибка');
  end;

  Text.Text := FNumberBeat.ToString;

  Inc(FNumberBeat);
  if FNumberBeat >= 9 then
    FNumberBeat := 1;

end;

procedure TNumberBitFrame.SetStart;
begin
  FNumberBeat := 1;
end;

end.
