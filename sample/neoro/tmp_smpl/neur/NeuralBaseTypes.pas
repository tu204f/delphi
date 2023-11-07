{*******************************************************}
{        ���������� ����������� ��������� �����         }
{          Neural Networks Component Library            }
{                                                       }
{           ����������� BaseGroup (c) 2000              }
{           Copyright (c) 2000 BaseGroup Lab            }
{*******************************************************}

unit NeuralBaseTypes;

interface

const

  DefaultAlpha = 1;              // �������� �������� ������������� �������
  DefaultEpochCount = 10000;     // ���������� ���� ��� ��������
  DefaultErrorValue = 0.05;      // ������ �� ��������� ��� ���� �����
  DefaultHopfLayerCount = 2;     // ���������� ����� � ���� ��������
  DefaultLayerCount = 0;         // ����������� ���������� ����� � ���� back-propagation
  DefaultMaxIterCount = 10;      // ������������ ���������� �������� � ��������� ��������
  DefaultMomentum = 0.9;         // ������� - ������
  DefaultNeuronCount = 0;        // ���������� �������� � ������� ���� �� ���������
  DefaultPatternCount = 0;       // ���������� ��������
  DefaultTeachRate = 0.1;        // �������� ��������
  DefaultTeachIdentCount = 100;  // ��������� ������� ������������ �������� �� ���������� ���������
  DefaultTestIdentCount = 100;   // ��������� ������� ������������ �������� �� ��������� ���������
  DefaultUseForTeach = 100;      // ������� �������� ������������ � �������� ���������� ���������
  DefaultDeltaBarAcceleratingConst = 0.095;
  DefaultDeltaBarDecFactor = 0.85;
  DefaultRPropInitValue = 0.1;
  DefaultRPropMaxStepSize = 50;
  DefaultRPropMinStepSize = 1E-6;
  DefaultRPropDecFactor = 0.5;
  DefaultRPropIncFactor = 1.2;
  DefaultSuperSABDecFactor = 0.5;
  DefaultSuperSABIncFactor = 1.05;

  SensorLayer = 0;               // ��������� ����
  BiasNeuron = 1;                // �������� � ���� back-propagation

  Separators = ['.', ','];
  Letters    = ['a'..'z'];
  Capitals   = ['A'..'Z'];
  DigitChars = ['0'..'9'];
  SpaceChar = #9;

resourcestring

  SFieldNorm = '�� ���������� ���� ������������ %d';
  SFieldKind = '�� ���������� ���� ���� %d';
  SFieldIndexRange = '����������� ������ ����� ���� %d';
  SInFieldCount = '����������� ����������� ���������� ������� �����';
  SInNeuronCount = '����������� ����������� ���������� ������� ��������';
  SInVectorCount = '����������� ����������� ����������� �������� �������';
  SLayerRangeIndex = '����������� ������ ����� ���� %d';
  SNeuronRangeIndex = '����������� ������ ����� ������� %d';
  SNeuronCount = '����������� ������� ���������� ��������';
  SOutFieldCount = '����������� ����������� ���������� �������� �����';
  SOutNeuronCount = '����������� ����������� ���������� �������� ��������';
  SOutVectorCount = '����������� ����������� ����������� ��������� �������';
  SPatternRangeIndex = '����� �� ������� ������� �������� %d';
  SStreamCannotRead = '������ ������ �� ������';
  SWeightRangeIndex = '����������� ������ ����� ���� %d';
  SWrongFileName = '����������� ������� ��� ����� %s';
  SCannotBeNumber = '������, ��������� %s ���������� �������� � ��������� ����';
  SBPStopCondition = '�� ������ ������� �������� �������� ��������';

type

  TVectorInt = array of integer;
  TVectorFloat = array of double;
  TVectorString = array of string;
  TMatrixInt = array of array of integer;
  TMatrixFloat = array of array of double;
  TNormalize = (nrmLinear, nrmSigmoid, nrmAuto,  nrmNone,
                nrmLinearOut, nrmAutoOut);
  TNeuroFieldType = (fdInput, fdOutput, fdNone);

implementation


end.

