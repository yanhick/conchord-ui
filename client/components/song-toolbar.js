import React from 'react';
import SongTextSize from './song-text-size';
import PushDownButton from './widgets/push-down-button';

export default React.createClass({

    render: function () {

        return (
            <ul>
                <SongTextSize
                    onIncrement={this.props.onIncrementFontSize}
                    onDecrement={this.props.onDecrementFontSize} />
                <PushDownButton
                    onClick={this.props.onToggleShowChords}
                    icon="folder"
                    active={this.props.showChords} />
                <PushDownButton
                    onClick={this.props.onToggleShowDuplicatedChoruses}
                    icon="folder"
                    active={this.props.showDuplicatedChoruses} />
            </ul>
        );
    }

});


