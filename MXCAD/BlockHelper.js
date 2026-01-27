/**
 * Block Helper Module
 * Provides utility functions for creating and managing blocks in MXCAD
 */

/**
 * Generates a unique block name with timestamp
 * @param {string} prefix - Block name prefix (e.g., "UChannel", "Table1")
 * @returns {string} - Unique block name
 */
window.generateBlockName = function (prefix) {
    const timestamp = Date.now();
    return `${prefix}_${timestamp}`;
}

/**
 * Creates a block from an array of entities and inserts it into the drawing
 * @param {string} blockName - Unique name for the block
 * @param {Array<McDbEntity>} entities - Array of entities to add to the block
 * @param {McGePoint3d} basePoint - Base point for the block (default: origin)
 * @param {McGePoint3d} insertPoint - Insertion point for the block reference
 * @returns {McObjectId|null} - Block reference ID or null if failed
 */
window.createBlockFromEntities = function (blockName, entities, basePoint, insertPoint) {
    const MX = window.MxCAD || {};
    const mxcad = MX.MxCpp.getCurrentMxCAD();

    if (!mxcad) {
        console.error("MXCAD not ready.");
        return null;
    }

    if (!entities || entities.length === 0) {
        console.warn("No entities provided to create block.");
        return null;
    }

    const { McDbBlockTableRecord, McDbBlockReference, McGePoint3d } = MX;
    const database = mxcad.getDatabase();
    const blockTable = database.getBlockTable();

    // Use origin as default base point
    const blockBasePoint = basePoint || new McGePoint3d(0, 0, 0);
    const blockInsertPoint = insertPoint || blockBasePoint;

    try {
        // Create new block table record
        const blockRecord = new McDbBlockTableRecord();
        blockRecord.name = blockName;
        blockRecord.origin = blockBasePoint;

        // Add block record to block table FIRST
        const blockRecordId = blockTable.add(blockRecord);

        if (!blockRecordId || blockRecordId.isNull()) {
            console.error("Failed to add block record to block table.");
            return null;
        }

        // Get the registered block record back from the database
        const registeredBlockRecord = blockRecordId.getMcDbBlockTableRecord();
        if (!registeredBlockRecord) {
            console.error("Failed to retrieve registered block record.");
            return null;
        }

        // Now add all entities to the REGISTERED block record
        entities.forEach(entity => {
            if (entity) {
                registeredBlockRecord.appendAcDbEntity(entity);
            }
        });

        // Create block reference
        const blockRef = new McDbBlockReference();
        blockRef.blockTableRecordId = blockRecordId;
        blockRef.position = blockInsertPoint;

        // Add block reference to current space
        const currentSpace = database.currentSpace;
        const blockRefId = currentSpace.appendAcDbEntity(blockRef);

        console.log(`Block '${blockName}' created successfully with ${entities.length} entities.`);

        return blockRefId;
    } catch (error) {
        console.error(`Error creating block '${blockName}':`, error);
        return null;
    }
}

/**
 * Creates a block from entities and updates the display
 * This is a convenience wrapper around createBlockFromEntities
 * @param {string} blockNamePrefix - Block name prefix
 * @param {Array<McDbEntity>} entities - Array of entities
 * @param {McGePoint3d} basePoint - Base point for the block
 * @param {McGePoint3d} insertPoint - Insertion point
 * @returns {McObjectId|null} - Block reference ID or null if failed
 */
window.createAndDrawBlock = function (blockNamePrefix, entities, basePoint, insertPoint) {
    const blockName = window.generateBlockName(blockNamePrefix);
    const MX = window.MxCAD || {};
    const mxcad = MX.MxCpp.getCurrentMxCAD();

    if (mxcad) {
        const resetOk = mxcad.newFile();
        if (!resetOk) {
            console.warn("Failed to reset drawing with newFile().");
        }
    }

    const blockRefId = window.createBlockFromEntities(blockName, entities, basePoint, insertPoint);

    if (blockRefId) {
        const blockRef = blockRefId.getMcDbEntity?.();
        if (mxcad && blockRef?.getBoundingBox) {
            const { minPt, maxPt, ret } = blockRef.getBoundingBox();
            if (ret) {
                mxcad.zoomW(minPt, maxPt);
            } else {
                console.warn("Failed to get block reference bounding box.");
            }
            mxcad.updateDisplay();
        }
    }

    return blockRefId;
}
