import DeviceDetector, { DetectResult } from 'node-device-detector';
export const detectDevice = (userAgent: string): DetectResult => new DeviceDetector().detect(userAgent);
