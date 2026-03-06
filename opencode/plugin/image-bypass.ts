import type { Plugin, Hooks, Message, Part } from "@opencode-ai/plugin";
import { appendFileSync, mkdirSync, writeFileSync } from "fs";
import { existsSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";

const LOG_ENABLED = false;

const LOG_FILE = "/tmp/opencode-debug.log";
const TEMP_DIR = "/tmp/opencode_images";

function log(message: string, data?: any) {
    if (!LOG_ENABLED) return;
    const timestamp = new Date().toISOString();
    const logEntry = `[${timestamp}] ${message}\n`;

    if (data) {
        appendFileSync(
            LOG_FILE,
            logEntry + JSON.stringify(data, null, 2) + "\n\n",
            "utf-8",
        );
    } else {
        appendFileSync(LOG_FILE, logEntry, "utf-8");
    }
}

function ensureTempDir() {
    if (!existsSync(TEMP_DIR)) {
        mkdirSync(TEMP_DIR, { recursive: true });
        log(`Created temp directory: ${TEMP_DIR}`);
    }
}

function saveImageToTempFile(
    dataUri: string,
    filename?: string,
): string | null {
    log (`filename: ${filename}`);
    if (filename && existsSync(filename)) {
        log (`Using existing file: ${filename}`);
        return filename;
    }
    try {
        log(`  Decoding image from data URI: ${dataUri.substring(0, 100)}...`);

        const match = dataUri.match(/^data:([^;]+);base64,(.+)$/);
        if (!match) {
            log(`  Failed to parse data URI`);
            return null;
        }

        const mimeType = match[1];
        const base64Data = match[2];
        const buffer = Buffer.from(base64Data, "base64");

        let ext = ".bin";
        if (mimeType.startsWith("image/png")) ext = ".png";
        else if (
            mimeType.startsWith("image/jpeg") ||
                mimeType.startsWith("image/jpg")
        )
            ext = ".jpg";
        else if (mimeType.startsWith("image/gif")) ext = ".gif";
        else if (mimeType.startsWith("image/webp")) ext = ".webp";
        else if (mimeType.startsWith("image/svg")) ext = ".svg";

        let tempFilename = `opencode_image_${new Date().toISOString().slice(0, 24).replace(/-/g, '')}_${Math.floor(Math.random() * 1000000).toString().padStart(6, '0')}${ext}`;
        const tempFilePath = path.join(TEMP_DIR, tempFilename);

        writeFileSync(tempFilePath, buffer);

        log(`  Saved image to temp file: ${tempFilePath}`);
        log(`  File size: ${buffer.length} bytes`);

        return tempFilePath;
    } catch (error) {
        log(`  Failed to save image to temp file:`, error);
        return null;
    }
}

function shouldSkipImageProcessing(output: any): boolean {
    const lastUserMsg = output.messages?.findLast((msg: any) => msg.info?.role === "user");

    const modelName = lastUserMsg?.info?.model?.modelID

    if (!modelName) {
        // 如果找不到模型信息，默认处理图像
        return false;
    }

    const isGLMVision = /^glm-.*v$/i.test(modelName);
    if (isGLMVision) {
        return true;
    }

    // 可选：仅对 GLM 模型处理图像，其他模型跳过
    const skipIfNotGLMModel = false;
    if (skipIfNotGLMModel) {
        if (!/^glm-/i.test(modelName)) {
            return true;
        }
    }
    return false;
}

export default async function ({ client }: PluginInput): Promise<Hooks> {
    log("=".repeat(80));
    log("IMAGE-BYPASS PLUGIN INITIALIZED - SCHEME K");
    log("=".repeat(80));

    ensureTempDir();

    return {
        "experimental.chat.messages.transform": async (input, output) => {
            log("-".repeat(80));
            log("experimental.chat.messages.transform HOOK CALLED");
            log("-".repeat(80));

            if (!output.messages || output.messages.length === 0) {
                log("No messages to process");
                return;
            }

            if (shouldSkipImageProcessing(output)) {
                const lastUserMsg = output.messages?.findLast((msg: any) => msg.info?.role === "user");
                log(`Skipping image processing for model: ${lastUserMsg?.info?.model?.modelID || 'unknown'}`);
                return;
            }

            let processedImages = 0;
            let totalUserMessages = 0;
            const tempFiles: string[] = [];

            try {
                for (const msg of output.messages) {
                    if (msg.info.role !== "user") continue;

                    totalUserMessages++;
                    if (!msg.parts || msg.parts.length === 0) continue;

                    log(
                        `\nProcessing message ${msg.info.id} with ${msg.parts.length} parts`,
                    );

                    // 策略：检测 type: "image" 的消息，替换为文本
                    const newParts: MessageV2.Part[] = [];

                    for (let i = 0; i < msg.parts.length; i++) {
                        const part = msg.parts[i];

                        log(`  Part ${i}:`, {
                            type: part.type,
                            mime: part.mime,
                            urlPreview: part.url?.substring(0, 100) || "null",
                            textPreview: part.text?.substring(0, 100),
                        });

                        // 检查是否是 type: "image" 的消息（可能由其他处理生成）
                        if (part.type === "image") {
                            log(`    Found type: "image" part, will replace with text`);

                            // 检查是否有图像数据
                            let imageData = part.image as string | undefined;
                            if (!imageData && part.url?.startsWith("data:")) {
                                imageData = part.url;
                            }

                            if (imageData) {
                                // 保存为临时文件
                                const tempFilePath = saveImageToTempFile(
                                    imageData,
                                    part.filename,
                                );

                                if (tempFilePath) {
                                    tempFiles.push(tempFilePath);

                                    // 替换为文本消息，告知文件位置
                                    const textMessage = `The local file path of the image is: ${tempFilePath}`;

                                    newParts.push({
                                        id: part.id,
                                        messageID: part.messageID,
                                        sessionID: part.sessionID,
                                        type: "text",
                                        text: textMessage,
                                    });

                                    processedImages++;
                                    log(`    Replaced image with text: "${textMessage}"`);
                                } else {
                                    // 保存失败，保留原样但改为文本
                                    newParts.push({
                                        id: part.id,
                                        messageID: part.messageID,
                                        sessionID: part.sessionID,
                                        type: "text",
                                        text: "[ERROR: Failed to save image to temporary file]",
                                    });
                                    log(`    Failed to save temp file`);
                                }
                            } else {
                                log(`    Image part has no data`);
                                newParts.push(part);
                            }
                        } else if (
                            part.type === "file" &&
                                part.mime?.startsWith("image/")
                        ) {
                            // 处理 type: "file" 的图像部分
                            log(`    Found file part with image mime`);

                            const originalUrl = part.url || "";

                            if (originalUrl.startsWith("data:")) {
                                // 保存为临时文件并替换为文本
                                const tempFilePath = saveImageToTempFile(
                                    originalUrl,
                                    part.filename,
                                );

                                if (tempFilePath) {
                                    tempFiles.push(tempFilePath);

                                    const textMessage = `The local file path of the image is: ${tempFilePath}`;

                                    newParts.push({
                                        id: part.id,
                                        messageID: part.messageID,
                                        sessionID: part.sessionID,
                                        type: "text",
                                        text: textMessage,
                                    });

                                    processedImages++;
                                    log(`    Replaced image file with text: "${textMessage}"`);
                                } else {
                                    newParts.push(part);
                                    log(`    Failed to save temp file, keeping original`);
                                }
                            } else {
                                // 非 data URI，可能已经是文件路径
                                newParts.push(part);
                                log(`    Non-data URI, keeping as-is`);
                            }
                        } else {
                            // 保留其他部分不变
                            newParts.push(part);
                        }
                    }

                    // 替换消息的所有部分
                    msg.parts = newParts;
                    log(`  Replaced all parts, new count: ${msg.parts.length}`);
                }

                log("-".repeat(80));
                log("SUMMARY:");
                log(`  Total user messages: ${totalUserMessages}`);
                log(`  Total images processed: ${processedImages}`);
                log(`  Temp files created: ${tempFiles.length}`);
                if (tempFiles.length > 0) {
                    log(`  Temp files:`, tempFiles);
                }
                log("-".repeat(80));
            } catch (error) {
                log("ERROR during processing:", error);
            }
        },
    };
}
